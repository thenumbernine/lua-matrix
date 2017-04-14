-- my attempt at a ffi implementation of the matrix class

local class = require 'ext.class'

-- here's my original pure-Lua version
local matrix_lua = require 'matrix'

local matrix_cpu = class()

-- override this to specify a default for the ctor ctype parameter 
matrix_cpu.real = nil

--[[
	constructors:
	matrix_cpu() = nil matrix_cpu {}
	matrix_cpu(t, env, ctype), 
		t can be
		- a (arbitrarily nested) table 
		- another matrix_cpu object
		env is the cl.obj.env. it is required unless matrix_cpu.env is already set.
		ctype is the buffer type.  default is env.real 

		size is derived from src, unless src is nil
		in which case size can specify the size of the matrix
		I really am only using it with matrix_cpu.zeros, not planning on it being public
--]]
function matrix_cpu:init(src, ctype, size)
	if type(src) == 'table' 
	and not matrix_cpu.is(src) 
	then
		src = matrix_lua(src)
	end

	if matrix_cpu.is(src) then
		self.size_ = src:size()
	elseif matrix_lua.is(src) then
		self.size_ = matrix_cpu(src:size())
	elseif size then
		self.size_ = matrix_cpu(size)
	else
		assert(src == nil)
		self.size_ = matrix_cpu{0}
	end
	
	self.volume = self.size_:prod()

	self.step = self.size_:size():zeros()
	for i=2,#self.size_ do
		self.step[i] = self.step[i-1] * self.size_[i]
	end

	self.ctype = ctype or self.real or self.env.real

	self.ptr = ffi.new(self.ctype..'[?]', math.max(self.volume,1))

	if matrix_cpu.is(src) then
		ffi.copy(self.ptr, src.ptr, ffi.sizeof(self.ctype) * self.volume)
	elseif src ~= nil then
		for i in src:iter() do
			self[i] = src[i]
		end
	end
end

-- sorry, for my matrix lib compat,
-- you gotta set ctypes with matrix_cpu.real = whatever ctype
function matrix_cpu.const(value, dim, ...)
	local m = matrix_cpu(nil, nil,
		type(dim) == 'table'
		and matrix_cpu(dim)
		or matrix_cpu{dim, ...})
	for i=0,self.volume-1 do
		self.ptr[i] = value
	end
	return m
end

-- matches matrix_lua except the matrix ref
function matrix_cpu.zeros(...)
	return matrix_cpu.const(0, ...)
end

-- matches matrix_lua except the matrix ref
function matrix_cpu.ones(...)
	return matrix_cpu.const(1, ...)
end

-- could match matrix_lua except the matrix ref, if I copied it back over, but it might be slightly slower?
function matrix_cpu.lambda(size, f)
	local size = matrix_cpu.is(size) and size or matrix_cpu(size)
	if size:degree() == 0 then return f() end
	local self = size:zeros()
	for i in self:iter() do
		self[i] = assert(f(i:unpack()))  
	end
	return self
end

-- returns the matrix size
function matrix_cpu:size()
	return matrix_cpu(self.size_)
end

function matrix_cpu:degree()
	return self.size_.volume
end

function matrix_cpu:toLuaMatrix()
	return matrix_lua.lambda({self.size_:unpack()}, function(...)
		return self(...)
	end)
end

function matrix_cpu:__tostring(n)
	return m:toLuaMatrix():__tostring(n)
end

-- matches matrix_lua 
function matrix_cpu.__concat(a,b)
	return tostring(a) .. tostring(b)
end

-- currently a lot more restricted than the lua version
-- only handles m(i1,...,iN) for N = the degree of the matrix m
-- no submatrix access, no slicing, etc.
function matrix_cpu:__call(...)
	local i = ...
	local n = select('#', ...)
	if type(i) == 'table' then error'TODO' end 
	assert(n == self:degree())
	local index = 0
	for j=1,n do
		index = index + (select(j, ...) - 1) * self.step[j]
	end
	return self.ptr[index]
end

-- matches matrix_lua except the matrix ref
function matrix_cpu:__index(i)
	if type(i) ~= 'table' then return rawget(self,i) or rawget(matrix_cpu,i) end
	return self(table.unpack(i))
end

-- same as __call
function matrix_cpu:__newindex(i,v)
	if type(i) == 'table' then error 'TODO' end
	rawset(self,i,v)
end

-- matches matrix_lua except the matrix ref
function matrix_cpu:iter()
	return coroutine.wrap(function()
		local size = self:size()
		local i = matrix_cpu.zeros(#size)
		for j=1,#size do
			i[j] = 1
		end
		repeat
			coroutine.yield(matrix_cpu(i))
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

-- matches matrix_lua except matrix ref
function matrix_cpu.scale(a,s)
	if type(a) == 'number' then return a * b end
	assert(type(s) == 'number')
	a = matrix_cpu(a)
	for i in a:iter() do
		a[i] = a[i] * s
	end
	return a
end

-- could match matrix_lua except matrix ref if I copied it back
function matrix_cpu.inner(a,b,metric,aj,bj)
	if type(a) == 'number' then
		if type(b) == 'number' then return a * b end
		return b:scale(a)
	elseif type(b) == 'number' then
		return a:scale(b)
	end
	aj = aj or a:degree()
	bj = bj or 1
	local sa = table{a:size():unpack()}
	local sb = table{b:size():unpack()}
	local ssa = table(sa)
	local saj = ssa:remove(aj)
	local ssb = table(sb)
	local sbj = ssb:remove(bj)
	assert(saj == sbj, "inner dimensions must be equal")
	local sc = table(ssa):append(ssb)
	return matrix_cpu.lambda(sc, function(...)
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

-- matches matrix_lua
matrix_cpu.__mul = matrix_cpu.inner

-- matches matrix_lua except matrix ref 
function matrix_cpu.__div(a,b)
	assert(matrix_cpu.is(a))
	assert(type(b) == 'number')
	return matrix_cpu.lambda(a:size(), function(...)
		return a(...) / b
	end)
end

-- matches matrix_lua except matrix ref 
function matrix_cpu.emul(a,b)
	if not matrix_cpu.is(a) and not matrix_cpu.is(b) then
		return a * b
	end
	if matrix_cpu.is(a) and not matrix_cpu.is(b) then
		return a:size():lambda(function(...)
			return a(...) * b
		end)
	end
	if not matrix_cpu.is(a) and matrix_cpu.is(b) then
		return b:size():lambda(function(...)
			return a * b(...)
		end)
	end
	assert(a:size() == b:size())
	return a:size():lambda(function(...)
		return a(...) * b(...)
	end)
end

-- matches matrix_lua except matrix ref 
function matrix_cpu.ediv(a,b)
	if not matrix_cpu.is(a) and not matrix_cpu.is(b) then
		return a / b
	end
	if matrix_cpu.is(a) and not matrix_cpu.is(b) then
		return a:size():lambda(function(...)
			return a(...) / b
		end)
	end
	if not matrix_cpu.is(a) and matrix_cpu.is(b) then
		return b:size():lambda(function(...)
			return a / b(...)
		end)
	end
	assert(a:size() == b:size())
	return a:size():lambda(function(...)
		return a(...) / b(...)
	end)
end

-- doesn't match, due to matrix_cpu lacking sub-element access
-- instead this sums all elements
function matrix_cpu:sum()
	local sum = self.ptr[0]
	for i=1,self.volume-1 do
		sum = sum + self.ptr[i]
	end
	return sum
end

-- same as :sum()
function matrix_cpu:prod()
	local prod = self.ptr[0]
	for i=1,self.volume-1 do
		prod = prod * self.ptr[i]
	end
	return prod
end

function matrix_cpu.dot(a,b)
	assert(a.size == b.size)
	local sum = 0
	for i=0,a.volume-1 do
		sum = sum + a.ptr[i] * b.ptr[i]
	end
	return sum
end

function matrix_cpu:normSq() return self:dot(self) end
function matrix_cpu:norm() return math.sqrt(self:normSq()) end

function matrix_cpu:normL1()
	local l = 0
	for i=0,self.volume-1 do
		l = l + math.abs(self.ptr[i])
	end
	return l
end

function matrix_cpu:normLInf()
	local l = 0
	for i=0,self.volume-1 do
		l = math.max(l, math.abs(self.ptr[i]))
	end
	return l
end

function matrix_cpu.__eq(a,b)
	if type(a) ~= type(b) then return false end
	if a.size_.volume ~= b.size_.volume then return false end
	for j=0,a.size_.volume-1 do
		if a.size_.ptr[j] ~= b.size_.ptr[j] then return false end
	end
	for i=0,a.volume-1 do
		if a.ptr[i] ~= b.ptr[i] then return false end
	end
	return true
end

function matrix_cpu:map(f)
	return self:size():lambda(function(...)
		return f(self(...), ...)
	end)
end

return matrix_cpu
