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

TODO this used to be compat with matrix_ffi, but then I added ctype to matrix_ffi.zeros, and that meant forcing only {dim1 ... dimN} ctor in matrix_ffi
 so then should I remove the (dim1, ..., dimN) signature from matrix.zeros?
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

-- expands the i'th dimension to i, i+1
function matrix:diag(i)
	i = i or 1
	self = matrix(self)
	local size = self:size()
	local newsize = matrix(size)
	table.insert(newsize, i, size[i])
	return newsize:lambda(function(...)
		local ji1 = select(i, ...)
		local ji2 = select(i+1, ...)
		if ji1 == ji2 then
			local srcj = table{...}
			srcj:remove(i)
			return self[srcj]
		else
			return 0
		end
	end)
end

function matrix.eye(size)
	if #size == 0 then return 1 end
	if #size == 1 then size[2] = size[1] end
	return matrix.lambda(size, function(i,j,...)
		if #size == 0 or #size == 1 then return 1 end
		return i==j and 1 or 0
	end)
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
	local typeSelf1 = type(self[1])
	if typeSelf1 == 'number' or typeSelf1 == 'string' or typeSelf1 == 'cdata' then
		for i=2,#self do
			local typeSelfI = type(self[i])
			assert(typeSelfI == 'number' or typeSelfI == 'string' or typeSelfI == 'cdata', "matrix had a bad dimension")
		end
	-- else if self[1] is something that can be indexed
	-- TODO what if it is cdata?  or a table?
	-- it could either be something indended as a value or something intended as iteration
	-- how to determine which is which?
	-- the most flexible way might be to make this test matrix:isa(self[1]) ...
	elseif self[1] ~= nil then
		for i=2,#self do
			assert(#self[1] == #self[i], "matrix had a bad dimension")
		end
		self[1]:size(sizes,offset+1)
	end
	return sizes
end

--[[
For matrix.ffi compat, since luajit is still 5.1 compat and doesn't have a __len overload operator
matrix.ffi overloads __len aka # operator for the matrix first dimension size.

Python uses 'len' as the size of an array / vector
Javascript uses 'length' for the length of a string or array.
C++ string uses 'length' and 'size'.  vector / containers use '.size'

Matlab uses 'length' as the length / max size of an object.
Matlab uses 'norm' as the L2 norm / vector norm / vector magnitude.

My lua vec, lua vec-ffi, cpp Tensor libs use .length() as the vector magnitude.

So after all that, should I add 'length' for compat with vec?
Nah, instead I'll add 'norm' to vec and vec-ffi (and Tensor?) as compat with Matlab and this.
--]]
function matrix:len()
	return #self
end

function matrix:degree()
	return #self:size()
end

function matrix:__tostring(n)
	n = n or self:degree()
	return '[' .. table(self):map(function(cell)
		if matrix:isa(cell) then
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
					self[k][{table.unpack(i,2)}] = type(v) == 'number' and v or v[j]
				else
					self[k] = type(v) == 'number' and v or v[j]
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
	if type(a) == 'number' then return a * s end
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
					--assert(type(ai) == 'number')
					--assert(type(bi) == 'number')
					sum = sum + metric[u][v] * ai * bi
				end
			end
		else
			for u=1,saj do
				ia[aj] = u
				ib[bj] = u
				local ai = a[ia]
				local bi = b[ib]
				--assert(type(ai) == 'number')
				--assert(type(bi) == 'number')
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
	assert(matrix:isa(a))
	assert(type(b) == 'number')
	return matrix.lambda(a:size(), function(...)
		return a(...) / b
	end)
end

-- per-element multiplication
function matrix.emul(a,b)
	if not matrix:isa(a) and not matrix:isa(b) then
		return a * b
	end
	if matrix:isa(a) and not matrix:isa(b) then
		return a:size():lambda(function(...)
			return a(...) * b
		end)
	end
	if not matrix:isa(a) and matrix:isa(b) then
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
	if not matrix:isa(a) and not matrix:isa(b) then
		return a / b
	end
	if matrix:isa(a) and not matrix:isa(b) then
		return a:size():lambda(function(...)
			return a(...) / b
		end)
	end
	if not matrix:isa(a) and matrix:isa(b) then
		return b:size():lambda(function(...)
			return a / b(...)
		end)
	end
	assert(a:size() == b:size())
	return a:size():lambda(function(...)
		return a(...) / b(...)
	end)
end

-- per-element exponent
function matrix.epow(a,b)
	if not matrix:isa(a) and not matrix:isa(b) then
		return a ^ b
	end
	if matrix:isa(a) and not matrix:isa(b) then
		return a:size():lambda(function(...)
			return a(...) ^ b
		end)
	end
	if not matrix:isa(a) and matrix:isa(b) then
		return b:size():lambda(function(...)
			return a ^ b(...)
		end)
	end
	assert(a:size() == b:size())
	return a:size():lambda(function(...)
		return a(...) ^ b(...)
	end)
end

-- sums all sub-elements in the matrix
function matrix:sum()
	local sum = matrix:isa(self[1]) and matrix(self[1]) or self[1]
	for i=2,#self do
		sum = sum + self[i]
	end
	return sum
end

function matrix:prod()
	local t = type(self[1])
	if t ~= 'number' then
		error("got a bad type: "..t)
	end
	local prod = self[1]
	for i=2,#self do
		prod = prod * self[i]
	end
	return prod
end

function matrix:min()
	local x = matrix:isa(self[1]) and self[1]:min() or self[1]
	for i=2,#self do
		if type(self[i]) == 'number' then
			x = math.min(x, self[i])
		else
			x = math.min(x, self[i]:min())
		end
	end
	return x
end

function matrix:max()
	local x = matrix:isa(self[1]) and self[1]:max() or self[1]
	for i=2,#self do
		if type(self[i]) == 'number' then
			x = math.max(x, self[i])
		else
			x = math.max(x, self[i]:max())
		end
	end
	return x
end

-- what is the name of this operation? it's dot for vectors.  it and itself on matrices is the Frobenius norm.
function matrix.dot(a,b)
	assert(#a == #b)
	local sum = 0
	for i=1,#a do
		local ai = a[i]
		if matrix:isa(ai) then
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

matrix.determinant = require 'matrix.determinant'
matrix.det = matrix.determinant

matrix.inverse = require 'matrix.inverse'
matrix.inv = matrix.inverse

-- these are vector-math specific:

--[[
levi-civita permutation matrix
pass a number n to make the tensor of size  {n,n,.. n} (n-times)
otherwise pass an array for arbitrary size
--]]
function matrix.levciv(...)
	local args = ...
	if type(args) == 'number' then
		if select('#', ...) > 1 then
			error("got some unexpected arguments")
		end
		args = table.rep({args}, args)
	end
	return matrix(args):lambda(function(...)
		local indexes = {...}
		-- duplicates mean 0
		for i=1,#indexes-1 do
			for j=i+1,#indexes do
				if indexes[i] == indexes[j] then return 0 end
			end
		end
		-- bubble sort, count the flips
		local parity = 1
		for i=1,#indexes-1 do
			for j=1,#indexes-i do
				if indexes[j] > indexes[j+1] then
					indexes[j], indexes[j+1] = indexes[j+1], indexes[j]
					parity = -parity
				end
			end
		end
		return parity
	end)
end

function matrix.cross(a, b)
	local na = #a
	local nb = #b
	-- [[ the fast way, for R3 x R3 cross
	if na == 3 and nb == 3 then
		return matrix{
			a[2] * b[3] - a[3] * b[2],
			a[3] * b[1] - a[1] * b[3],
			a[1] * b[2] - a[2] * b[1],
		}
	end
	--]]
	-- [[ the slow way, for any dimension.
	-- notice there's a free parameter of the result dimension, I just picked the max
	return b * matrix.levciv{nb, math.max(na, nb), na} * a
	--]]
end

function matrix.rotate(theta, nx, ny, nz)
	local I = matrix.eye{3}
	local K = matrix{{0, -nz, ny}, {nz, 0, -nx}, {-ny, nx, 0}}
	--local K = -matrix{nx, ny, nz} * matrix.levciv(3)
	local K2 = K * K
	return I
		+ K * math.sin(theta)
		+ K2 * (1 - math.cos(theta))
end

function matrix.unit(m)
	return m / m:norm()
end

-- naming compat with the vec library
matrix.normalize = matrix.unit

return matrix
