-- my attempt at a ffi implementation of the matrix class
-- take note, this relies on luajit
-- and luajit doesn't overload __len
local ffi = require 'ffi'
local class = require 'ext.class'
local table = require 'ext.table'

-- here's my original pure-Lua version
local matrix_lua = require 'matrix'

local matrix_ffi = class()

-- override this to specify a default for the ctor ctype parameter 
matrix_ffi.real = nil

--[[
	constructors:
	matrix_ffi() = nil matrix_ffi {}
	matrix_ffi(t, ctype, size, env), 
		t can be
		- a (arbitrarily nested) table 
		- another matrix_ffi object
		env is the cl.obj.env. it is required unless matrix_ffi.env is already set.
		ctype is the buffer type.  default is env.real 

		size is derived from src, unless src is nil
		in which case size can specify the size of the matrix
		I really am only using it with matrix_ffi.zeros, not planning on it being public
--]]
function matrix_ffi:init(src, ctype, size)
	if type(src) == 'table' 
	and not matrix_ffi.is(src) 
	then
		src = matrix_lua(src)
	end
	-- make sure src is a matrix_ffi or matrix_lua

	-- the size member can't be a matrix_ffi or we'll get infinite recursion ...
	-- so I could make it a regular table
	-- but why not a matrix_lua, so we get some similar functionality?
	if matrix_ffi.is(src) then
		self.size_ = matrix_lua{src:size():unpack()}
	elseif matrix_lua.is(src) then
		self.size_ = src:size()
	elseif size then
		if matrix_ffi.is(size) then
			self.size_ = matrix_lua{size:unpack()}
		else
			self.size_ = matrix_lua(size)
		end
	else
		assert(src == nil)
		self.size_ = matrix_lua{0}
	end

	self.volume = self.size_:prod()

	self.step = matrix_lua(self.size_)
	self.step[1] = 1
	for i=2,#self.size_ do
		self.step[i] = self.step[i-1] * self.size_[i-1]
	end

	self.ctype = ctype or self.real or 'double'

	self.ptr = ffi.new(self.ctype..'[?]', math.max(self.volume,1))

	if matrix_ffi.is(src) then
		ffi.copy(self.ptr, src.ptr, ffi.sizeof(self.ctype) * self.volume)
	elseif src ~= nil then
		for i in src:iter() do
			self[i] = src[i]
		end
	end
end

-- sorry, for my matrix lib compat,
-- you gotta set ctypes with matrix_ffi.real = whatever ctype
function matrix_ffi.const(value, dim, ...)
	local self = matrix_ffi(nil, nil,
		type(dim) == 'table'
		and matrix_ffi(dim)
		or matrix_ffi{dim, ...})
	for i=0,self.volume-1 do
		self.ptr[i] = value
	end
	return self
end

-- matches matrix_lua except the matrix ref
function matrix_ffi.zeros(...)
	return matrix_ffi.const(0, ...)
end

-- matches matrix_lua except the matrix ref
function matrix_ffi.ones(...)
	return matrix_ffi.const(1, ...)
end

-- could match matrix_lua except the matrix ref, if I copied it back over, but it might be slightly slower?
function matrix_ffi.lambda(size, f, result)
	local size = matrix_ffi.is(size) 
		and size or matrix_ffi(size)
	if size:degree() == 0 then return f() end
	if not result then result = size:zeros() end
	for i in result:iter() do
		local x = assert(f(i:unpack()))  
		result[i] = x
	end
	return result
end

-- returns the matrix size
-- size_ is stored as a matrix_lua, but :size() will return a matrix_ffi
function matrix_ffi:size()
	return matrix_ffi(self.size_)
end

function matrix_ffi:len()
	return self.size_[1]
end

function matrix_ffi:degree()
	return #self.size_
end

function matrix_ffi:toLuaMatrix()
	return matrix_lua.lambda({self.size_:unpack()}, function(...)
		return self(...)
	end)
end

function matrix_ffi:__tostring(n)
	return self:toLuaMatrix():__tostring(n)
end

-- matches matrix_lua 
function matrix_ffi.__concat(a,b)
	return tostring(a) .. tostring(b)
end

-- currently a lot more restricted than the lua version
-- only handles m(i1,...,iN) for N = the degree of the matrix m
-- no submatrix access, no slicing, etc.
function matrix_ffi:__call(...)
	local i = ...
	local n = select('#', ...)
	if type(i) == 'table' then error'TODO' end 
	local deg = self:degree()
	assert(n <= deg, "tried to index too far into a matrix")
	
	if n == deg then
		local index = 0
		for j=1,n do
			index = index + (select(j, ...) - 1) * self.step[j]
		end
		return self.ptr[index]
	else	-- n < deg
		local size = self:size():toLuaMatrix()	-- switch to matrix_lua to use slicing (which I haven't added to matrix_ffi yet ...)
		local newsize = matrix_ffi{deg-n}:lambda(function(j)
			return size[n+j]
		end)
		local m = newsize:zeros()
		for i in newsize:range() do
			local j = matrix_ffi{deg}:zeros()
			for k=1,n do
				j[k] = select(k, ...)
			end
			for k=n+1,deg do
				j[k] = i[k-n]
			end
			m[i] = self[j]
		end
		return m
	end
end

-- matches matrix_lua except the matrix ref
function matrix_ffi:__index(i)
	if type(i) == 'number' then 
		if self:degree() == 1 then
			return self.ptr[i-1]
		else
			return self(i)
		end
	end
	if type(i) ~= 'table' then 
		return rawget(self,i) or rawget(matrix_ffi,i) 
	end
	if matrix_ffi.is(i) then
		return self(i:unpack())
	end
	return self(table.unpack(i))
end

function matrix_ffi:getindex(i)
	assert(matrix_lua.is(i) or matrix_ffi.is(i))
	if self.size_:len() ~= i:len() then
		error('tried to getindex differently-ranked vectors i',i,'and size',self.size_)
	end
	local index = 0
	for j=1,i:len() do
		if not (i[j] >= 1 and i[j] <= self.size_[j]) then
			error("got out of bounds index: "..i)
		end
		index = index + (i[j] - 1) * self.step[j]
	end
if index < 0 or index >= self.volume then
	print('bad index',index,'of volume',self.volume)
	print('i',i)
	print('size',self.size_)
	print('step',self.step)
	error'here'
end
	return index
end

function matrix_ffi:__newindex(i,v)
	if type(i) == 'table' then
		-- make sure i is a matrix_lua
		if matrix_ffi.is(i) then 
			i = i:toLuaMatrix() 
		elseif not matrix_lua.is(i) then
			i = matrix_lua(i)
		end
	elseif type(i) == 'number' then
		i = matrix_lua{i}
	else
		rawset(self,i,v)
		return
	end
	
	if type(v) == 'table' then
		-- make sure v is a matrix_ffi
		if not matrix_ffi.is(v) then 
			v = matrix_ffi(v) 
		end
		for j in v:iter() do
			local t = matrix_lua()
			for k=1,#i do t[k] = i[k] end
			for k=1,j.volume do t[k+#i] = j[k] end
			self.ptr[self:getindex(t)] = v[j]
		end
	elseif type(v) == 'number' then
		self.ptr[self:getindex(i)] = v
	else
		error("can only assign numbers or tables, not "..type(v))
	end
end

function matrix_ffi.__unm(a)
	local c = matrix_ffi(a)
	for i=0,c.volume-1 do
		c.ptr[i] = -c.ptr[i]
	end
	return c
end

function matrix_ffi.__add(a,b)
	local c = matrix_ffi(a)
	if type(b) == 'number' then
		for i=0,c.volume-1 do
			c.ptr[i] = c.ptr[i] + b
		end
	elseif matrix_ffi.is(b) then
		assert(c.size_ == b.size_)
		for i=0,c.volume-1 do
			c.ptr[i] = c.ptr[i] + b.ptr[i]
		end
	else
		for i=1,c.volume do
			c.ptr[i-1] = c.ptr[i-1] + b[i]
		end
	end
	return c
end

function matrix_ffi.__sub(a,b)
	local c = matrix_ffi(a)
	if type(b) == 'number' then
		for i=0,c.volume-1 do
			c.ptr[i] = c.ptr[i] - b
		end
	elseif matrix_ffi.is(b) then
		assert(c.size_ == b.size_)
		for i=1,c.volume do
			c.ptr[i-1] = c.ptr[i-1] - b.ptr[i-1]
		end
	else
		for i=1,c.volume do
			c.ptr[i-1] = c.ptr[i-1] - b[i]
		end
	end
	return c
end


-- matches matrix_lua except the matrix ref
function matrix_ffi:range()
	return coroutine.wrap(function()
		local i = matrix_ffi.zeros(self.volume)
		for j=1,self.volume do
			i[j] = 1
		end
		repeat
			coroutine.yield(matrix_ffi(i))
			for j=1,i.volume do
				i[j] = i[j] + 1
				if i[j] <= self[j] then break end
				i[j] = 1
				if j == i.volume then
					return
				end
			end
		until nil 
	end)
end

function matrix_ffi:iter() 
	return self:size():range() 
end


-- matches matrix_lua except matrix ref
function matrix_ffi.scale(a,s)
	if type(a) == 'number' then return a * b end
	assert(type(s) == 'number')
	a = matrix_ffi(a)
	for i in a:iter() do
		a[i] = a[i] * s
	end
	return a
end

-- could match matrix_lua except matrix ref if I copied it back
function matrix_ffi.inner(a,b,metric,aj,bj, c)
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

	if c then
		assert(#c.size_ == #sc)
		for i=1,#sc do
			assert(c.size_[i] == sc[i])
		end
	else
		c = matrix(nil, a.ctype, sc)
	end

	return matrix_ffi.lambda(sc, function(...)
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
	end, c)
end

-- matches matrix_lua
matrix_ffi.__mul = matrix_ffi.inner

-- this is here to put the result arg first
-- I want to put it first in all cases
-- but this would break compat with matrix_lua, and with the * operator
function matrix_ffi.mul(c, a,b,metric,aj,bj)
	return matrix_ffi.__mul(a,b,metric,aj,bj, c)
end

-- matches matrix_lua except matrix ref 
function matrix_ffi.__div(a,b)
	assert(matrix_ffi.is(a))
	assert(type(b) == 'number')
	return matrix_ffi.lambda(a:size(), function(...)
		return a(...) / b
	end)
end

-- matches matrix_lua except matrix ref 
function matrix_ffi.emul(a,b)
	if not matrix_ffi.is(a) and not matrix_ffi.is(b) then
		return a * b
	end
	if matrix_ffi.is(a) and not matrix_ffi.is(b) then
		return a:size():lambda(function(...)
			return a(...) * b
		end)
	end
	if not matrix_ffi.is(a) and matrix_ffi.is(b) then
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
function matrix_ffi.ediv(a,b)
	if not matrix_ffi.is(a) and not matrix_ffi.is(b) then
		return a / b
	end
	if matrix_ffi.is(a) and not matrix_ffi.is(b) then
		return a:size():lambda(function(...)
			return a(...) / b
		end)
	end
	if not matrix_ffi.is(a) and matrix_ffi.is(b) then
		return b:size():lambda(function(...)
			return a / b(...)
		end)
	end
	assert(a:size() == b:size())
	return a:size():lambda(function(...)
		return a(...) / b(...)
	end)
end

-- doesn't match, due to matrix_ffi lacking sub-element access
-- instead this sums all elements
function matrix_ffi:sum()
	local sum = self.ptr[0]
	for i=1,self.volume-1 do
		sum = sum + self.ptr[i]
	end
	return sum
end

-- same as :sum()
function matrix_ffi:prod()
	local prod = self.ptr[0]
	for i=1,self.volume-1 do
		prod = prod * self.ptr[i]
	end
	return prod
end

function matrix_ffi.dot(a,b)
	assert(matrix_ffi.is(a))
	assert(matrix_ffi.is(b))
	assert(a.size_ == b.size_)
	local sum = 0
	for i=0,a.volume-1 do
		sum = sum + a.ptr[i] * b.ptr[i]
	end
	return sum
end

function matrix_ffi:normSq() return self:dot(self) end
function matrix_ffi:norm() return math.sqrt(self:normSq()) end

function matrix_ffi:normL1()
	local l = 0
	for i=0,self.volume-1 do
		l = l + math.abs(self.ptr[i])
	end
	return l
end

function matrix_ffi:normLInf()
	local l = 0
	for i=0,self.volume-1 do
		l = math.max(l, math.abs(self.ptr[i]))
	end
	return l
end

function matrix_ffi.__eq(a,b)
	if type(a) ~= type(b) then return false end
	if a.size_.volume ~= b.size_.volume then return false end
	if a.size_ ~= b.size_ then return false end
	for i=0,a.volume-1 do
		if a.ptr[i] ~= b.ptr[i] then return false end
	end
	return true
end

function matrix_ffi:map(f)
	return self:size():lambda(function(...)
		return f(self(...), ...)
	end)
end

function matrix_ffi:unpack()
	local t = {}
	for i=0,self.volume-1 do
		t[i+1] = self.ptr[i]
	end
	return table.unpack(t)
end

return matrix_ffi
