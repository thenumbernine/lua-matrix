--[[
this isn't really a matrix library
more of a n-ary array of numbers library.  vector, matrix.
not necessarily tensors, as they imply invariance to coordinate transform, which is one assumption too far for a title.
--]]

local class = require 'ext.class'
local table = require 'ext.table'
local _ = require 'matrix.index'

local matrix = class()

matrix.index = _

function matrix:init(t, ...)
	if type(t) == 'table' then
		for i=1,#t do
			if type(t[i]) == 'table' then
				self[i] = matrix(t[i])
			else
				self[i] = t[i]
			end
		end
	end
end

--[[
static initializer
matrix.const(value, dim1, ..., dimN)
matrix.const(value, {dim1, ..., dimN})
--]]
function matrix.const(value, dim, ...)
	local self = matrix()
	local subdegree = select('#', ...)
	local rows = {}
	if type(dim) == 'table' then
		return matrix.const(value, table.unpack(dim))
	else
		assert(type(dim) == 'number')
		for i=1,dim do
			if subdegree == 0 then
				self[i] = value
			else
				self[i] = matrix.const(value, ...)
			end
		end
		return self
	end
end

--[[
initialize an empty matrix
matrix.zeros(dim1, ..., dimN)
matrix.zeros{dim1, ..., dimN}
the latter lets you use a matrix to initialize a matrix: matrix{2,2}:zeros() produces {{0,0},{0,0}}
--]]
function matrix.zeros(...)
	return matrix.const(0, ...)
end

function matrix.ones(...)
	return matrix.const(1, ...)
end

-- static initializer
-- matrix.lambda({dim1, ..., dimN}, function(i1, ..., iN) ... end)
function matrix.lambda(size, f)
	if #size == 0 then return f() end
	local self = matrix.zeros(table.unpack(size))
	for i in self:iter() do
		local x = assert(f(table.unpack(i)))
		self[i] = x
	end
	return self
end

--[[
should the matrix {} have size return {} or {0} ?  technically the latter implies a vector of size 0 (whereas {0,0} would be a matrix of size 0,0)
and the matrix {} would not necessarily be a vector, let alone anything
looks like matlab always returns two values for size
I wonder what numpy does
what about nested nils? {1,2,3} is of size {3}, {{1},{2},{3}} is of size {3,1}, so  {{}, {}, {}} should be of size {3,0}
 however if {} had size {} then {{}, {}, {}} would have size of just {3}, which is ambiguous
therefore {} should return size {0}
--]]
function matrix:size(sizes, offset)
	offset = offset or 1
	sizes = sizes or matrix{}
	sizes[offset] = #self
	if type(self[1]) == 'number' then
		for i=2,#self do
			assert(type(self[i]) == 'number', "matrix had a bad dimension")
		end
	elseif self[1] ~= nil then
		for i=2,#self do
			assert(#self[1] == #self[i], "matrix had a bad dimension")
		end
		self[1]:size(sizes,offset+1)
	end
	return sizes
end

-- for matrix_ffi compat, since luajit is still 5.1 compat and doesn't have a __len overload operator
function matrix:len()
	return #self
end

function matrix:degree()
	return #self:size()
end

function matrix:__tostring(n)
	n = n or self:degree()
	return '[' .. table(self):map(function(cell)
		if matrix.is(cell) then
			return cell:__tostring(n-1)
		end
		return tostring(cell)
	end):concat(n>1 and ',\n' or ', ') .. ']'
end

function matrix.__concat(a,b)
	return tostring(a) .. tostring(b)
end

--[[
used for matrix slicing
see test.lua for examples

if m = {{1,2,3},
		{4,5,6},
		{7,8,9}}

then m({1,3},{1,3}) gives
	{{1,3},
	 {7,9}}

and m({1,2}, {1,2}) gives
	{{1,2},
	 {4,5}}

notice that the _ operator acts as a shorthand for contiguous regions
so m(_(1,3),{1}) gives {{1},{4},{7}}

also nesting or something
so m(_(1,3),1) gives {1,4,7}

--]]
function matrix:__call(i, ...)
	if i == _ then i = _(#self) end
	if type(i) == 'table' then
		local copy = matrix()
		for j,k in ipairs(i) do
			copy[j] = self(k,...)
		end
		return copy
	end
	if select('#', ...) == 0 then
		return self[i]
	end
	return self[i](...)
end

function matrix:__index(i)
	if type(i) ~= 'table' then return rawget(self,i) or rawget(matrix,i) end
	return self(table.unpack(i))
end

function matrix:__newindex(i,v)
	if i == _ then i = {_(#self)} end	-- range ref should only be used from outside this method
	if type(i) == 'table' then
		local ii = i[1]
		if ii == _ then ii = _(#self) end
		if type(ii) == 'number' then
			if #i > 1 then
				self[ii][{table.unpack(i,2)}] = v
			else
				self[ii] = v
			end
		else
			for j,k in ipairs(ii) do
				if #i > 1 then
					self[k][{table.unpack(i,2)}] = type(v) == 'number' and v or v[k]
				else
					self[k] = type(v) == 'number' and v or v[k]
				end
			end
		end
	else
		rawset(self,i,v)
	end
end

function matrix.__unm(a)
	local c = matrix(a)
	for i=1,#c do
		c[i] = -c[i]
	end
	return c
end

function matrix.__add(a,b)
	local c = matrix(a)
	if type(b) == 'number' then
		for i=1,#c do
			c[i] = c[i] + b
		end
	else
		for i=1,#c do
			c[i] = c[i] + b[i]
		end
	end
	return c
end

function matrix.__sub(a,b)
	local c = matrix(a)
	if type(b) == 'number' then
		for i=1,#c do
			c[i] = c[i] - b
		end
	else
		for i=1,#c do
			c[i] = c[i] - b[i]
		end
	end
	return c
end

function matrix:range()
	return coroutine.wrap(function()
		local i = matrix.zeros(#self)
		for j=1,#self do
			i[j] = 1
		end
		repeat
			coroutine.yield(matrix(i))
			for j=1,#i do
				i[j] = i[j] + 1
				if i[j] <= self[j] then break end
				i[j] = 1
				if j == #i then
					return
				end
			end
		until nil 
	end)
end

function matrix:iter() 
	return self:size():range() 
end

function matrix.scale(a,s)
	if type(a) == 'number' then return a * b end
	assert(type(s) == 'number')
	a = matrix(a)
	for i in a:iter() do
		a[i] = a[i] * s
	end
	return a
end

-- non-metatable version will be matrix mul 
-- assumes the innermost dim of a equals the outermost dim of b
function matrix.outer(a,b)
	if type(a) == 'number' then
		if type(b) == 'number' then return a * b end
		return b:outer(a)
	end
	a = matrix(a)
	for i in a:iter() do
		a[i] = b:scale(a[i])
	end
	return a
end

--[[
a,b = matrices
metric = metric to perform inner product, default = identity
aj,bj = degrees to contract, default = last of a, first of b
--]]
function matrix.inner(a,b,metric,aj,bj)
	if type(a) == 'number' then
		if type(b) == 'number' then return a * b end
		return b:scale(a)
	elseif type(b) == 'number' then
		return a:scale(b)
	end
	aj = aj or a:degree()
	bj = bj or 1
	local sa = a:size()
	local sb = b:size()
	local ssa = table(sa)
	local saj = ssa:remove(aj)
	local ssb = table(sb)
	local sbj = ssb:remove(bj)
	assert(saj == sbj, "inner dimensions must be equal")
	local sc = table(ssa):append(ssb)
	return matrix.lambda(sc, function(...)
		local i = {...}
		local ia = table{table.unpack(i,1,#sa-1)}
		ia:insert(aj,'false')
		local ib = table{table.unpack(i,#sa)}
		ib:insert(bj,'false')
		local sum = 0
		if metric then
			for u=1,saj do
				for v=1,sbj do
					ia[aj] = u
					ib[bj] = v
					local ai = a[ia]
					local bi = b[ib]
					assert(type(ai) == 'number')
					assert(type(bi) == 'number')
					sum = sum + metric[u][v] * ai * bi
				end
			end
		else
			for u=1,saj do
				ia[aj] = u
				ib[bj] = u
				local ai = a[ia]
				local bi = b[ib]
				assert(type(ai) == 'number')
				assert(type(bi) == 'number')
				sum = sum + ai * bi
			end
		end
		return sum
	end)
end

-- scalar multiplication
-- maybe per-component
-- probably not outer or inner by default
-- or maybe inner <=> matrix multiplication / dot products?
matrix.__mul = matrix.inner

-- scalar division
function matrix.__div(a,b)
	assert(matrix.is(a))
	assert(type(b) == 'number')
	return matrix.lambda(a:size(), function(...)
		return a(...) / b
	end)
end

-- per-element multiplication
function matrix.emul(a,b)
	if not matrix.is(a) and not matrix.is(b) then
		return a * b
	end
	if matrix.is(a) and not matrix.is(b) then
		return a:size():lambda(function(...)
			return a(...) * b
		end)
	end
	if not matrix.is(a) and matrix.is(b) then
		return b:size():lambda(function(...)
			return a * b(...)
		end)
	end
	assert(a:size() == b:size())
	return a:size():lambda(function(...)
		return a(...) * b(...)
	end)
end

-- per-element division
function matrix.ediv(a,b)
	if not matrix.is(a) and not matrix.is(b) then
		return a / b
	end
	if matrix.is(a) and not matrix.is(b) then
		return a:size():lambda(function(...)
			return a(...) / b
		end)
	end
	if not matrix.is(a) and matrix.is(b) then
		return b:size():lambda(function(...)
			return a / b(...)
		end)
	end
	assert(a:size() == b:size())
	return a:size():lambda(function(...)
		return a(...) / b(...)
	end)
end

-- sums all sub-elements in the matrix
function matrix:sum()
	local sum = matrix(self[1])
	for i=2,#self do
		sum = sum + self[i]
	end
	return sum
end

function matrix:prod()
	assert(type(self[1]) == 'number')
	local prod = self[1]
	for i=2,#self do
		prod = prod * self[i]
	end
	return prod
end

-- what is the name of this operation? it's dot for vectors.  it and itself on matrices is the Frobenius norm.  
function matrix.dot(a,b)
	assert(#a == #b)
	local sum = 0
	for i=1,#a do
		local ai = a[i]
		if matrix.is(ai) then
			sum = sum + ai:dot(b[i])
		else
			sum = sum + ai * b[i]
		end
	end
	return sum
end

function matrix:normSq() return self:dot(self) end

-- Frobenius norm
function matrix:norm()
	return math.sqrt(self:normSq())
end

function matrix:normL1()
	local l = 0
	for i in self:iter() do
		l = l + math.abs(self[i])
	end
	return l
end

function matrix:normLInf()
	local l = 0
	for i in self:iter() do
		l = math.max(l, math.abs(self[i]))
	end
	return l
end

function matrix.__eq(a,b)
	if type(a) ~= type(b) then return false end
	if #a ~= #b then return false end
	for i=1,#a do
		if a[i] ~= b[i] then return false end
	end
	return true
end

function matrix:transpose(aj,bj)
	--dimensions to transpose
	aj = aj or 1
	bj = bj or 2
	local size = self:size()
	size[aj], size[bj] = size[bj], size[aj]
	return matrix.lambda(size, function(...)
		local si = {...}
		si[aj], si[bj] = si[bj], si[aj]
		return self[si]
	end)
end

function matrix:T(...)
	return self:transpose(...)
end

function matrix:map(f)
	return self:size():lambda(function(...)
		return f(self(...), ...)
	end)
end

-- for convenience
-- should I made this table.unpack, or should I flatten all subtables as well?
-- should I make a reshape() function?
matrix.unpack = table.unpack

return matrix
