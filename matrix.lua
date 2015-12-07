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

-- static initializer
-- matrix.zeros(dim1, ..., dimN)
function matrix.zeros(dim, ...)
	local self = matrix()
	local subdegree = select('#', ...)
	local rows = {}
	assert(type(dim) == 'number')
	for i=1,dim do
		if subdegree == 0 then
			self[i] = 0
		else
			self[i] = matrix.zeros(...)
		end
	end
	return self
end

-- static initializer
-- matrix.lambda(dim1, ..., dimN, function(i1, ..., iN) ... end)
function matrix.lambda(...)
	local size = {...}
	local f = table.remove(size)
	assert(type(f) == 'function')
	if #size == 0 then return f() end
	local self = matrix.zeros(table.unpack(size))
	for i in self:iter() do
		self[i] = f(table.unpack(i)) 
	end
	return self
end

function matrix:size(sizes, offset)
	offset = offset or 1
	sizes = sizes or {}
	sizes[offset] = #self
	if type(self[1]) == 'number' then
		for i=2,#self do
			assert(type(self[i]) == 'number', "matrix had a bad dimension")
		end
	else
		for i=2,#self do
			assert(#self[1] == #self[i], "matrix had a bad dimension")
		end
		self[1]:size(sizes,2)
	end
	return matrix(sizes)
end

function matrix:degree()
	return #self:size()
end

function matrix:__tostring(n)
	n = n or self:degree()
	return '[' .. table(self):map(function(cell)
		if getmetatable(cell) == matrix then
			return cell:__tostring(n-1)
		end
		return tostring(cell)
	end):concat(n>1 and ',\n' or ', ') .. ']'
end

function matrix.__concat(a,b)
	return tostring(a) .. tostring(b)
end

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

function matrix.__add(a,b)
	local c = matrix(a)
	for i=1,#c do
		c[i] = c[i] + b[i]
	end
	return c
end

function matrix.__sub(a,b)
	local c = matrix(a)
	for i=1,#c do
		c[i] = c[i] - b[i]
	end
	return c
end

function matrix:iter()
	return coroutine.wrap(function()
		local size = self:size()
		local i = matrix.zeros(#size)
		for j=1,#size do
			i[j] = 1
		end
		repeat
			coroutine.yield(table.unpack(i))
			for j=1,#i do
				i[j] = i[j] + 1
				if i[j] <= size[j] then break end
				i[j] = 1
				if j == #i then
					return
				end
			end
		until nil 
	end)
end

function matrix.scale(a,s)
	assert(type(s) == 'number')
	if type(a) == 'number' then return a * b end
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
	return matrix.lambda(table.unpack(sc), function(...)
		local i = {...}
		local ai = table{table.unpack(i,1,#sa-1)}
		ai:insert(aj,nil)
		local bi = table{table.unpack(i,#sa)}
		bi:insert(bj,nil)
		local sum = 0
		if metric then
			for u=1,saj do
				for v=1,sbj do
					ai[#sa] = u
					bi[1] = v
					sum = sum + a[ai] * b[bi] * metric[u][v]
				end
			end
		else
			for u=1,saj do
				ai[#sa] = u
				bi[1] = u
				sum = sum + a[ai] * b[bi]
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

-- Frobenius norm
function matrix:norm()
	local sum = 0
	for i=1,#self do
		local x = self[i]
		if matrix.is(x) then x = x:norm() end
		sum = sum + x * x
	end
	return math.sqrt(sum)
end

function matrix.__eq(a,b)
	if type(a) ~= type(b) then return false end
	if #a ~= #b then return false end
	for i=1,#a do
		if a[i] ~= b[i] then return false end
	end
	return true
end

return matrix
