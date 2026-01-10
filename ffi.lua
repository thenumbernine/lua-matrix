-- my attempt at a ffi implementation of the matrix class
-- take note, this relies on luajit
local ffi = require 'ffi'
local class = require 'ext.class'
local table = require 'ext.table'
local assert = require 'ext.assert'

local float = ffi.typeof'float'
local double = ffi.typeof'double'
-- TODO pcall?  in case its not supported?
local complex_float = ffi.typeof'complex float'
local complex_double = ffi.typeof'complex double'

local complex
local function requireComplex()
	complex = require 'complex'
	return complex
end

-- here's my original pure-Lua version
local matrix_lua = require 'matrix'

local matrix_ffi = class()

-- override this to specify a default for the ctor ctype parameter
-- TODO just call this .ctype and use it as a fallback field
matrix_ffi.real = double

local function isnumber(x)
	local xt = type(x)
	return xt == 'number' or xt == 'cdata'
end

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
function matrix_ffi:init(src, ctype, size, rowmajor)
--DEBUG:print('matrix_ffi:init', src, ctype, size, rowmajor)
	if type(src) == 'table'
	and not matrix_ffi:isa(src)
	then
		src = matrix_lua(src)
	end
	-- make sure src is a matrix_ffi or matrix_lua

	-- the size member can't be a matrix_ffi or we'll get infinite recursion ...
	-- so I could make it a regular table
	-- but why not a matrix_lua, so we get some similar functionality?
	local size_
	if matrix_ffi:isa(src) then
--DEBUG:print('...matrix_ffi:init getting size from src type matrix_ffi')
--DEBUG:print('...src:size():unpack() = ', src:size():unpack())
		size_ = matrix_lua{src:size():unpack()}
--DEBUG:print('...self.size_ = ', size_)
	elseif matrix_lua:isa(src) then
--DEBUG:print('...matrix_ffi:init getting size from src type matrix_lua')
--DEBUG:print('...src:size() = ', src:size())
		size_ = src:size()
--DEBUG:print('...self.size_ = ', size_)
	elseif size then
		if matrix_ffi:isa(size) then
--DEBUG:print('...matrix_ffi:init getting size from size type matrix_ffi')
			size_ = matrix_lua{size:unpack()}
		elseif type(size) == 'table' then
--DEBUG:print('...matrix_ffi:init getting size from size type table')
			size_ = matrix_lua{table.unpack(size)}
		else
--DEBUG:print('...matrix_ffi:init getting size from size type '..type(size))
			size_ = matrix_lua{size}
		end
	else
--DEBUG:print('...matrix_ffi:init getting size from nil size type '..type(size))
		assert.eq(src, nil)
		size_ = matrix_lua{0}
	end
--DEBUG:print('...size is', self.size_)
	local degree = #size_
	rawset(self, 'size_', size_)
	local volume = size_:prod()
	rawset(self, 'volume',  volume)

	-- TODO make rowmajor default.  make colmajor optional? for glsl's sake? or just nah?
	if rowmajor == nil and src and src.rowmajor ~= nil then
		rowmajor = src.rowmajor
	end
	rawset(self, 'rowmajor', rowmajor)
	local step = matrix_lua(size_)
	if rowmajor then
		step[#size_] = 1
		for i=#size_-1,1,-1 do
			step[i] = step[i+1] * size_[i+1]
		end
	else
		step[1] = 1
		for i=2,#size_ do
			step[i] = step[i-1] * size_[i-1]
		end
	end
	rawset(self, 'step', step)

	ctype = ffi.typeof(
		ctype 			-- ctype arg
		or (src and src.ctype) 	-- src ctype
		or self.real 			-- default ctype ... TODO why not matrix_ffi.ctype?
	)
	rawset(self, 'ctype', ctype)

	-- if we're building a matrix with complex ctype ...
	if tostring(ctype):lower():find'complex' then
		-- then make sure complex ctypes have their metamethods defined
		requireComplex()
	end

	local ctypeArray = ffi.typeof('$[?]', ctype)
	local ptr = ctypeArray(math.max(volume, 1))
	rawset(self, 'ptr', ptr)

	if matrix_ffi:isa(src) then
		local srcdegree = src:degree()
		-- NOTICE this only works if all rowmajors of all children match as well ...
		if (
			-- degree-1, vectors, copy, doesn't matter
			(
				degree == 1
				and srcdegree == 1
			)
			-- degree-2 will work so long as rowmajor matches
			or (
				degree == 2
				and srcdegree == 2
				and rowmajor == src.rowmajor
			)
			-- for degree >= 3, we need to ensure *all* children's rowmajor matches in order to use ffi.copy...
		) then
			local srcptr = rawget(src, 'ptr')
--DEBUG:print('...matrix_ffi:init reading src as matrix_ffi')
			if src.ctype == ctype then
--DEBUG:print('...matrix_ffi:init reading src as matrix_ffi with matching ctype -- ffi.copy')
				ffi.copy(ptr, srcptr, ffi.sizeof(ctype) * volume)
			else
--DEBUG:print('...matrix_ffi:init reading src as matrix_ffi with differing ctype -- iterate and assign')
				local mn = math.min(volume, src.volume)
				for i=0,mn-1 do
					ptr[i] = srcptr[i]
				end
				if mn < volume then
					local zero = ctype()
					for i=mn,volume-1 do
						ptr[i] = zero
					end
				end
			end
			-- bail out, don't use our generic number-filler below
			return
		end
	end

	-- if we didn't return then use our generic init here
	if src == nil then return end

--DEBUG:print('...matrix_ffi:init reading src as non-nil')
	for i in src:iter() do
		self[i] = src[i]
	end
end

function matrix_ffi.cast(A, ctype)
	return matrix_ffi(A, ctype)
end

-- sorry, for my matrix lib compat,
-- you gotta set ctypes with matrix_ffi.real = whatever ctype
function matrix_ffi.const(value, dims, ctype, ...)
--DEBUG:print('matrix_ffi.const', value, dims, ctype, ...)
	ctype = ctype or dims.ctype
	assert.type(dims, 'table')
	if ctype ~= nil then
		ctype = ffi.typeof(ctype)
	end
--DEBUG:print('...matrix_ffi.const dims src', table.unpack(dims))
	local dimsMat = matrix_ffi(dims)
--DEBUG:print('...matrix_ffi.const dims mat', dimsMat)
	local result = matrix_ffi(nil, ctype, dimsMat, ...)
	for i=0,result.volume-1 do
		result.ptr[i] = value
	end
--DEBUG:print('...matrix_ffi.const returning', result.size_)
	return result
end

-- matches matrix_lua except the matrix ref
function matrix_ffi.zeros(...)
--DEBUG:print('matrix_ffi.zeros', ...)
	if type((...)) == 'number' then
		local result = matrix_ffi.const(0, {...})
--DEBUG:print('...matrix_ffi.zeros(', ..., ') returning', result.size_)
		return result
	else
		local dims, ctype = ...
		ctype = ctype or dims.ctype
		if ctype ~= nil then ctype = ffi.typeof(ctype) end
		local result = matrix_ffi.const(0, dims, ctype, select(3, ...))
--DEBUG:print('...matrix_ffi.zeros(', ..., ') returning', result.size_)
		return result
	end
end

-- matches matrix_lua except the matrix ref
function matrix_ffi.ones(dims, ctype)
	return matrix_ffi.const(1, dims, ctype)
end

-- could match matrix_lua except the matrix ref, if I copied it back over, but it might be slightly slower?
function matrix_ffi.lambda(size, f, result, ctype, rowmajor)
--DEBUG(@5):print('matrix_ffi.lambda '..size)
	--[[ this might be compatible with matrix.lambda
	size = matrix_ffi:isa(size)
		and size
		or matrix_ffi(size)
	ctype = ctype or size.ctype
	if ctype ~= nil then
		ctype = ffi.typeof(ctype)
	end
	if size:degree() == 0 then return f() end
	if not result then result = size:zeros(ctype) end
	for i in result:iter() do
		local x = assert(f(i:unpack()))
		result[i] = x
	end
	return result
	--]]
	-- [[
	ctype = ctype or size.ctype
	-- if rowmajor wasn't provided then use size's rowmajor
	if rowmajor == nil then rowmajor = size.rowmajor end
	result = result or matrix_ffi(nil, ctype, size, rowmajor)
	if #size == 1 then
		local rptr = result.ptr
--DEBUG:assert.type(rptr, 'cdata')
		for i=0,size[1]-1 do
--DEBUG:assert.le(0, i)
--DEBUG:assert.lt(i, result.volume)
			rptr[i] = f(i+1)
		end
	-- [=[
	elseif #size == 2 then
		local rptr = result.ptr
--DEBUG:assert.type(rptr, 'cdata')
		local h = result.size_[1]
		local w = result.size_[2]
		local k = 0
		if not rowmajor then
			for j=0,w-1 do
				for i=0,h-1 do
--DEBUG:assert.le(0, k)
--DEBUG:assert.lt(k, result.volume)
					rptr[k] = f(i+1, j+1)
					k = k + 1
				end
			end
		else
			for i=0,h-1 do
				for j=0,w-1 do
--DEBUG:assert.le(0, k)
--DEBUG:assert.lt(k, result.volume)
					rptr[k] = f(i+1, j+1)
					k = k + 1
				end
			end
		end
	else
	--]=]
		-- sloooow ... and is it even working?
		--for i in result:iter() do
		for i in size:range() do
			result[i] = assert(f(i:unpack()))
		end
	end
	return result
	--]]
end

-- returns the matrix size
-- size_ is stored as a matrix_lua, but :size() will return a matrix_ffi
-- HMM why re-wrap size_ with matrix_ffi ... maybe I should just return a matrix() of size_ ?
function matrix_ffi:size()
	return matrix_ffi(self.size_
		-- if I use ctype here then it gets forwarded into lambda
		-- but honestly a size's ctype should be an integer while a lambda's should be a floating point
		--, self.ctype
	)
end

-- TODO rename this.
-- it's too ambiguous with vec.length()
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
--DEBUG:assert.type(rawget(self, 'ptr'), 'cdata')
	local firstI = ...
	local n = select('#', ...)
	if type(firstI) == 'table' then error'TODO' end
	local deg = self:degree()
	assert.le(n, deg, "tried to index too far into a matrix")

	if n == deg then
		local index = 0
		for j=1,n do
			index = index + (select(j, ...) - 1) * self.step[j]
		end
--DEBUG:assert.le(0, index)
--DEBUG:assert.le(index, rawget(self, 'volume'))
		return self.ptr[index]
	else	-- n < deg
		local size = self.size_
--DEBUG:assert.is(size, matrix_lua)
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
	if type(i) == 'string' then return getmetatable(self)[i] end	-- if it's a string ... and by here it isn't a field in the object ... then maybe it's a field in the metatable?
--DEBUG:assert.type(rawget(self, 'ptr'), 'cdata', "failed to __index "..require 'ext.tolua'(i))
	if type(i) == 'number' then
		if self:degree() == 1 then
--DEBUG:assert.le(1, i)
--DEBUG:assert.le(i, rawget(self, 'volume'))
			return self.ptr[i-1]
		else
			-- [[ readwrite, but only single-element at a time ...
			if self.rowmajor then
				if i < 1 or i > self.size_[1] then
					error("got OOB index "..tostring(i)..", should be within 1 and "..tostring(self.size_[1]))
				end
--DEBUG:assert.le(1, i)
--DEBUG:assert.le(i, rawget(self, 'size_')[1])
				-- TODO this is all a copy of matrix_ffi:init
				-- how about instead I work in this ptr as an arg in place of 'src' somehow ...
				-- or a better TODO would be to just throw out this whole class and replace it with a pure-ffi class that only used 0-index access and didn't have any Lua tables
				local m = setmetatable({}, matrix_ffi)
				m.size_ = matrix_lua{select(2, table.unpack(self.size_))}
				m.ctype = self.ctype or self.real
				m.volume = m.size_:prod()
	-- TODO step should be 1 for the j'th and big for 1st
	-- otherwise slicing like this won't work ...
				m.step = matrix_lua(m.size_)
				m.step[#m.size_] = 1
				for j=#m.size_-1,1,-1 do
					m.step[j] = m.step[j+1] * m.size_[j+1]
				end
				m.rowmajor = true
				if m.ctype:lower():find'complex' then
					requireComplex()
				end
--DEBUG:assert.le(m.volume, self.volume)
--DEBUG:assert.le(0, m.volume * (i-1))
--DEBUG:assert.lt(m.volume * (i-1), self.volume)
				m.ptr = self.ptr + m.volume * (i-1)
				return m
			--]]
			else
			-- [[ read-only w/slicing ...
				return self(i)
			--]]
			end
			return m
		end
	end
	if type(i) ~= 'table' then
		return rawget(self,i) or rawget(matrix_ffi,i)
	end
	if matrix_ffi:isa(i) then
		return self(i:unpack())
	end
	return self(table.unpack(i))
end

function matrix_ffi:getindex(i)
	assert(matrix_lua:isa(i) or matrix_ffi:isa(i))
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
		if matrix_ffi:isa(i) then
			i = i:toLuaMatrix()
		elseif not matrix_lua:isa(i) then
			i = matrix_lua(i)
		end
	elseif type(i) == 'number' then
		if self:degree() == 1 then
			assert.le(1, i, '__newindex is oob')
			assert.le(i, self.size_[1], '__newindex is oob')
			self.ptr[i-1] = v
			return
		else
			i = matrix_lua{i}
		end
	else
		rawset(self,i,v)
		return
	end

	local vt = type(v)
	if vt == 'table' then
		-- make sure v is a matrix_ffi
		if not matrix_ffi:isa(v) then
			v = matrix_ffi(v)
		end
		for j in v:iter() do
			local t = matrix_lua()
			for k=1,#i do t[k] = i[k] end
			for k=1,j.volume do t[k+#i] = j[k] end
			self.ptr[self:getindex(t)] = v[j]
		end
	elseif vt == 'number'
	or vt == 'cdata'	-- TODO only matching ffi.typeof(v) == self.ctype?
	then
		self.ptr[self:getindex(i)] = v
	else
		error("can only assign numbers or tables, not "..require 'ext.tolua'(v))
	end
end

-- I am jumping through such hoops just to make this matrix API match the pure-Lua matrix API
function matrix_ffi:__len()
	return self.size_[1]
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
	if isnumber(b) then
		for i=0,c.volume-1 do
			c.ptr[i] = c.ptr[i] + b
		end
	elseif matrix_ffi:isa(b) then
		assert.eq(c.size_, b.size_)
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
	if isnumber(b) then
		for i=0,c.volume-1 do
			c.ptr[i] = c.ptr[i] - b
		end
	elseif matrix_ffi:isa(b) then
		assert.eq(c.size_, b.size_)
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
		local i = matrix_ffi{self.volume}:zeros()
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
	return self.size_:range()
end


-- matches matrix_lua except matrix ref
-- TODO make an in-place option
function matrix_ffi.scale(a, s, result)
	if isnumber(a) then return a * s end
	assert(isnumber(s))
	result = result or matrix_ffi(a)
	for i in a:iter() do
		result[i] = a[i] * s
	end
	return result
end

-- could match matrix_lua except matrix ref if I copied it back
-- TODO this is opposite of matrix.mul4x4
-- ... I think esp for the optimized pathways
-- ... I think esp since I switched from row to col major memory as default ...
function matrix_ffi.inner(a,b,metric,aj,bj, c)
	if isnumber(a) then
		if isnumber(b) then return a * b end
		return b:scale(a, c)
	elseif isnumber(b) then
		return a:scale(b, c)
	end
	local dega = a:degree()
	local degb = b:degree()
--DEBUG:print('dega', dega, 'degb', degb)
	if aj then
		assert.le(1, aj)
		assert.le(aj, dega)
	else
		aj = dega
	end
	if bj then
		assert.le(1, bj)
		assert.le(bj, degb)
	else
		bj = 1
	end

	-- some optimized pathways
	if dega == 1 then
		if not matrix_ffi:isa(a) then a = matrix_ffi(a) end
		if not matrix_ffi:isa(b) then b = matrix_ffi(b) end
		local n = a.size_[1]
		if degb == 1 then
			-- inner product
			-- this is gonna break compat with matrix_lua/matrix_ffi mul
			-- but I don't want to create another temp obj ...
			assert.eq(n, b.size_[1])
			local sum = 0
			for i=0,n-1 do
				sum = sum + a.ptr[i] * b.ptr[i]
			end
			return sum
		elseif degb == 2 then
			assert.eq(n, b.size_[bj])
			local m = b.size_[3-bj]
			-- transposed mul
			-- should the result be rowmajor?
			-- what a mess ... I should pick a single standard and stick to it
			-- and for slicing sake it should be row-major
			if c then
				assert.eq(c:degree(), 1)
				assert.eq(c.size_[1], m)
			else
				c = matrix_ffi(nil, a.ctype, {m}, a.rowmajor)
			end

--DEBUG:print'multiplying'
--DEBUG:print(a)
--DEBUG:print'and'
--DEBUG:print(b)
--DEBUG:print':'
			if bj == 1 then	-- vec inner index1 of mat
--DEBUG:print('contract with 1st index')
				if not b.rowmajor then -- vec inner index1 of column-major mat = dot with cols
--DEBUG:print('2nd arg is col major mat')
					for j=0,n-1 do
						local sum = 0
						for i=0,m-1 do
--DEBUG:print('reading lhs index', i, 'value', a.ptr[i])
--DEBUG:print('reading rhs index', i, j, 'value', b.ptr[i + m * j])
							sum = sum + a.ptr[i] * b.ptr[i + m * j]
--DEBUG:print('sum is now', sum)
						end
						c.ptr[j] = sum
					end
				else	-- vec inner index1 of row-major mat = step by col width
--DEBUG:print('2nd arg is row major mat')
					for j=0,n-1 do
						local sum = 0
						for i=0,m-1 do
--DEBUG:print('reading lhs index', i, 'value', a.ptr[i])
--DEBUG:print('reading rhs index', i, j, 'value', b.ptr[j + m * i])
							sum = sum + a.ptr[i] * b.ptr[j + m * i]
--DEBUG:print('sum is now', sum)
						end
						c.ptr[j] = sum
					end
				end
			elseif bj == 2 then	-- vec inner index2 of mat
--DEBUG:print('contract with 2st index')
				if b.rowmajor then	-- vec inner index2 of row-major mat = dot with rows
--DEBUG:print('2nd arg is row major')
					for i=0,m-1 do
						local sum = 0
						for j=0,n-1 do
--DEBUG:print('reading lhs index', i, 'value', a.ptr[i])
--DEBUG:print('reading rhs index', i, j, 'value', b.ptr[i + m * j])
							sum = sum + a.ptr[i] * b.ptr[i + m * j]
--DEBUG:print('sum is now', sum)
						end
						c.ptr[i] = sum
					end
				else	-- vec inner index2 of col-major mat = step by row width
--DEBUG:print('2nd arg is col major')
					for i=0,m-1 do
						local sum = 0
						for j=0,n-1 do
--DEBUG:print('reading lhs index', i, 'value', a.ptr[i])
--DEBUG:print('reading rhs index', i, j, 'value', b.ptr[j + m * i])
							sum = sum + a.ptr[i] * b.ptr[j + m * i]
--DEBUG:print('sum is now', sum)
						end
						c.ptr[i] = sum
					end
				end
			else
				error'how did you get here?'
			end
			return c
		end
	elseif dega == 2 then
		if degb == 1 then
			-- TODO swap a and b and swap aj and bj
		elseif degb == 2 then
			if aj == 2 and bj == 1 then
				local a1, a2 = table.unpack(a.size_)
				local b1, b2 = table.unpack(b.size_)
				assert.eq(a2, b1)
				local c1 = a1
				local c2 = b2
				if c then
					assert.eq(c:degree(), 2)
					assert.eq(c.size_[1], c1)
					assert.eq(c.size_[2], c2)
				else
					c = matrix_ffi(nil, a.ctype, {c1, c2}, a.rowmajor)
				end

				if a.rowmajor ~= b.rowmajor then
					for i=0,a1-1 do
						for j=0,b2-1 do
							local sum = 0
							for k=0,a2-1 do
								sum = sum + a.ptr[i * a2 + k] * b.ptr[k * b2 + j]
							end
							c.ptr[i * c2 + j] = sum
						end
					end
					return c
				else
					for i=0,a1-1 do
						for j=0,b2-1 do
							local sum = 0
							for k=0,a2-1 do
								sum = sum + a.ptr[i + a1 * k] * b.ptr[k + b1 * j]
							end
							c.ptr[j * c2 + i] = sum
						end
					end
					return c
				end
			end
			-- TODO same
		end
	end

	local sa = table{a:size():unpack()}
	local sb = table{b:size():unpack()}
	local ssa = table(sa)
	local saj = ssa:remove(aj)
	local ssb = table(sb)
	local sbj = ssb:remove(bj)
	assert.eq(saj, sbj, "inner dimensions must be equal")

	local sc = table(ssa):append(ssb)

	if c then
		assert.eq(#c.size_, #sc)
		for i=1,#sc do
			assert.eq(c.size_[i], sc[i])
		end
	else
		c = matrix_ffi(nil, a.ctype, sc)
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
					assert(isnumber(ai))
					assert(isnumber(bi))
					sum = sum + metric[u][v] * ai * bi
				end
			end
		else
			for u=1,saj do
				ia[aj] = u
				ib[bj] = u
				local ai = a[ia]
				local bi = b[ib]
				assert(isnumber(ai))
				assert(isnumber(bi))
				sum = sum + ai * bi
			end
		end
		return sum
	end, c)
end

-- matches matrix_lua
-- TODO THIS IS TRANSPOSED as a result of me switching matrix.ffi from row to col major or vice versa ...
-- FIXME
matrix_ffi.__mul = matrix_ffi.inner

-- this is here to put the result arg first
-- I want to put it first in all cases
-- but this would break compat with matrix_lua, and with the * operator
function matrix_ffi.mul(c, a,b,metric,aj,bj)
	return matrix_ffi.__mul(a,b,metric,aj,bj, c)
end

-- matches matrix_lua except matrix ref
function matrix_ffi.__div(a,b)
	assert(matrix_ffi:isa(a))
	assert(isnumber(b))
	return matrix_ffi.lambda(a:size(), function(...)
		return a(...) / b
	end)
end

-- matches matrix_lua except matrix ref
function matrix_ffi.emul(a,b)
	if not matrix_ffi:isa(a) and not matrix_ffi:isa(b) then
		return a * b
	end
	if matrix_ffi:isa(a) and not matrix_ffi:isa(b) then
		return a:size():lambda(function(...)
			return a(...) * b
		end)
	end
	if not matrix_ffi:isa(a) and matrix_ffi:isa(b) then
		return b:size():lambda(function(...)
			return a * b(...)
		end)
	end
	assert.eq(a:size(), b:size())
	return a:size():lambda(function(...)
		return a(...) * b(...)
	end)
end

-- matches matrix_lua except matrix ref
function matrix_ffi.ediv(a,b)
	if not matrix_ffi:isa(a) and not matrix_ffi:isa(b) then
		return a / b
	end
	if matrix_ffi:isa(a) and not matrix_ffi:isa(b) then
		return a:size():lambda(function(...)
			return a(...) / b
		end)
	end
	if not matrix_ffi:isa(a) and matrix_ffi:isa(b) then
		return b:size():lambda(function(...)
			return a / b(...)
		end)
	end
	assert.eq(a:size(), b:size())
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
	assert(matrix_ffi:isa(a))
	assert(matrix_ffi:isa(b))
	assert.eq(a.size_, b.size_)
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

-- [[ copied from matrix.lua
function matrix_ffi:transpose(aj,bj)
	--dimensions to transpose
	aj = aj or 1
	bj = bj or 2
	local size = self:size()
--DEBUG(@5):io.write('matrix_ffi.transpose src size '..size, '\n')
	size[aj], size[bj] = size[bj], size[aj]
--DEBUG(@5):io.write('matrix_ffi.transpose dst size '..size, '\n')
	return matrix_ffi.lambda(size, function(...)
--DEBUG(@5):io.write('matrix_ffi:transpose')
		local si = {...}
--DEBUG(@5):io.write(' writing ', require 'ext.tolua'(si), ' ')
		si[aj], si[bj] = si[bj], si[aj]
--DEBUG(@5):io.write(' reading ', require 'ext.tolua'(si), ' ')
--DEBUG(@5):io.write(require 'ext.tolua'(self[si]))
--DEBUG(@5):print()
		return self[si]
	end, nil, self.ctype)
end

function matrix_ffi:T(...)
	return self:transpose(...)
end
--]]

function matrix_ffi:hermitian(...)
	local result = self:transpose(...)
	-- TODO a faster way to determine if the ctype is complex ...
	if self.ctype:lower():find'complex' then
		-- then make sure complex ctypes have their metamethods defined
		requireComplex()
		result = result:map(function(x)
			return complex.conj(x)
		end)
	end
	return result
end

function matrix_ffi:H(...)
	return self:hermitian(...)
end

function matrix_ffi:map(f, ctype, rowmajor)
	ctype = ctype or self.ctype
	if rowmajor == nil then rowmajor = self.rowmajor end
	return self:size():lambda(function(...)
		return f(self(...), ...)
	end, nil, ctype, rowmajor)
end

-- TODO unpack into a single Lua table?
-- or for matrix.unpack compat, unpack only the outermost dim into submatrices that are matrix_ffi ?
function matrix_ffi:unpack()
	local t = {}
	for i=0,self.volume-1 do
		t[i+1] = self.ptr[i]
	end
	return table.unpack(t)
end

function matrix_ffi:diag(i)
	i = i or 1
	local size = table{self:size():unpack()}
	local newsize = table(size)
	newsize:insert(i, size[i])
	return matrix_ffi.lambda(newsize, function(...)
		local ji1 = select(i, ...)
		local ji2 = select(i+1, ...)
		if ji1 == ji2 then
			local srcj = table{...}
			srcj:remove(i)
			return self[srcj]
		else
			return self.ctype()
		end
	end, nil, self.ctype)
end

function matrix_ffi.eye(size, ctype, ...)
	size = matrix_ffi(size)
	if size.volume == 0 then return 1 end
	local m,n
	if size.volume == 1 then
		m, n = size[1], size[1]
	else
		m, n = size[1], size[2]
	end
	ctype = ctype or size.ctype
	return matrix_ffi.lambda({m, n}, function(i,j)
		return i == j and 1 or 0
	end, nil, ctype, ...)
end

------------ LAPACKE SUPPORT ------------
-- should I put this in its own file?
-- right now it only tries to load lapacke if any functions are called, so I dont think separating it into another file is necessary

-- NOTICE I'm not handling any type for 'complex' alone, which luajit has synonymous for 'complex double'
-- so maybe I'll change the matrix_ffi ctor to swap that out

local scalarTypeForComplexType = {
	[float] = float,
	[double] = double,
	[complex_float] = float,
	[complex_double] = double,
}

local lapackeLetterForType = {
	[float] = 's',
	[double] = 'd',
	[complex_float] = 'c',
	[complex_double] = 'z',
}

local function getLapackeNameForType(ctype, name)
	--[[ lookup doesn't always work because type-equality doesn't imply object-hash-equality
	local letter = lapackeLetterForType[ctype]
	local scalarType = scalarTypeForComplexType[ctype]
	--]]
	-- [[ so instead...
	local letter, scalarType
	local ffitype = ffi.typeof(ctype)
	for checkctype, checkletter in pairs(lapackeLetterForType) do
		if ffi.typeof(checkctype) == ffitype then
			letter = checkletter
			scalarType = assert.index(scalarTypeForComplexType, checkctype)
			break
		end
	end
	--]]
	if not letter then
		error("can't find the lapacke letter associated with the ctype "..tostring(ctype))
	end
	return 'LAPACKE_'..letter..name, scalarType
end


--[[
perform a 3x3 svd
for the sake of a 3x3 matrix-exponent
https://www.ibm.com/docs/en/essl/6.2?topic=llss-sgesvd-dgesvd-cgesvd-zgesvd-sgesdd-dgesdd-cgesdd-zgesdd-singular-value-decomposition-general-matrix
--]]
function matrix_ffi.svd(A)
	local lapacke = require 'ffi.req' 'lapacke'
	A = matrix_ffi(A)	-- don't modify original
--print('A.ctype', A.ctype)
	local size = A:size()
	assert(matrix_ffi:isa(size))
	local svdName, scalarType = getLapackeNameForType(A.ctype, 'gesvd')
	local m, n = size:unpack()
	local U = matrix_ffi(nil, A.ctype, size, A.rowmajor)
--print('U.ctype', U.ctype)
	-- TODO or just remove the 'complex' from the type, if it is there
	if not scalarType then
		error("can't find scalar type for C type "..A.ctype)
	end
	local S = matrix_ffi(nil, scalarType, {m}, A.rowmajor)
--print('S.ctype', S.ctype)
	local VT = matrix_ffi(nil, A.ctype, size, A.rowmajor)
--print('VT.ctype', VT.ctype)
	local scalarTypeArray2 = ffi.tyepof('$[2]', scalarType)
	local superb = scalarTypeArray2() -- ... ???
	lapacke[svdName](
		A.rowmajor and lapacke.LAPACK_ROW_MAJOR or lapacke.LAPACK_COL_MAJOR,	-- int matrix_layout,
		('A'):byte(),				-- char jobu,	-- all m columns (the left singluar vectors) are returned in array u.
		('A'):byte(),				-- char jobvt,	-- all n rows (the right singular vectors) are returned in array vt.
		m,							-- int m,
		n,							-- lapack_int n,
		A.ptr,						-- double* a,
		m,							-- int lda,
		S.ptr,						-- double* s,
		U.ptr,						-- double* u,
		m,							-- lapack_int ldu,
		VT.ptr,						-- double* vt,
		n,							-- int ldvt,
		superb)						-- double* superb
--print('S\n'..S)
--print('VT\n'..VT)
	return U, S, VT:T()
end

-- Avr = λBvr
function matrix_ffi.eig(A, B)
	local lapacke = require 'ffi.req' 'lapacke'
	A = matrix_ffi(A)	-- don't modify original
	local eigName = getLapackeNameForType(A.ctype, 'ggev')
	local size = A:size()
	assert(matrix_ffi:isa(size))
	local m, n = size:unpack()
	assert.eq(m, n)
	B = B or size:eye(A.ctype, A.rowmajor)
	local alpha = matrix_ffi(nil, A.ctype, {n}, A.rowmajor)
	local beta = matrix_ffi(nil, A.ctype, {n}, A.rowmajor)
	local VL = matrix_ffi(nil, A.ctype, size, A.rowmajor)
	local VR = matrix_ffi(nil, A.ctype, size, A.rowmajor)
	local alphai
	-- too bad, I was really hoping all lapack functions of matching suffixes has matching # of args
	local Affitype = ffi.typeof(A.ctype)
	if Affitype == complex_float
	or Affitype == complex_double
	then
--print("cplx path")
		lapacke[eigName](
			A.rowmajor and lapacke.LAPACK_ROW_MAJOR or lapacke.LAPACK_COL_MAJOR,	-- int matrix_layout,
			('V'):byte(),				-- char jobvl,
			('V'):byte(),				-- char jobvr,
			n,							-- int n,
			A.ptr,						-- float __complex__* a,
			n,							-- lapack_int lda,
			B.ptr,						-- float __complex__* b,
			n,							-- int ldb,
			alpha.ptr,					-- float __complex__* alpha,
			beta.ptr,					-- float __complex__* beta,
			VL.ptr,						-- lapack_complex_float* vl,
			n,							-- int ldvl,
			VR.ptr,						-- float __complex__* vr,
			n)							-- int ldvr
	elseif Affitype == float
	or Affitype == double
	then
--print("real path")
		alphai = matrix_ffi(nil, A.ctype, {n}, A.rowmajor)
		lapacke[eigName](
			A.rowmajor and lapacke.LAPACK_ROW_MAJOR or lapacke.LAPACK_COL_MAJOR,	-- int matrix_layout,
			('V'):byte(),	-- char jobvl,
			('V'):byte(),	-- char jobvr,
			n,				-- int n,
			A.ptr,			-- double* a,
			n,				-- lapack_int lda,
			B.ptr,			-- double* b,
			n,				-- int ldb,
			alpha.ptr,		-- double* alphar,
			alphai.ptr,		-- double* alphai,
			beta.ptr,		-- double* beta,
			VL.ptr,			-- double* vl,
			n,				-- int ldvl,
			VR.ptr,			-- double* vr,
			n)				-- int ldvr
	else
		error("here")
	end
	return alpha, VR, VL, beta, alphai
end

function matrix_ffi.inv(A)
	A = matrix_ffi(A)	-- don't modify original
	-- hmm seems there is a ?geicd that is like getri but also returns the determinant,
	--  but I'm not seeing it in LAPACKE, maybe just LAPACK?
	-- why do I use LAPACKE if matrix.ffi is only col-major anyways?
	-- TODO add row-major optional support to matrix.ffi by reversing the step[] table
	local size = A:size()
	assert(matrix_ffi:isa(size))
	local m, n = size:unpack()
	if m ~= n then
		error("expected square matrix, got dimensions "..m..', '..n)
	end
	assert.eq(m, n)	-- needed for getri
	local mn = math.min(m,n)
	local ipiv = matrix_ffi(nil, 'int', {mn}, A.rowmajor)

	-- ok lapacke is sometimes the ffi.cdef that 'table overflow's luajit
	-- meaning i'm including too much in luajit
	-- so lets avoid it if possible ...
	if m == 2 then
		local a = A.ptr[0 + 2 * 0]
		local b = A.ptr[0 + 2 * 1]
		local c = A.ptr[1 + 2 * 0]
		local d = A.ptr[1 + 2 * 1]
		local invdet = 1 / (a * d - b * c)
		-- A_ij pos is i + m * j
		A.ptr[0 + 2 * 0] = d * invdet
		A.ptr[0 + 2 * 1] = -c * invdet
		A.ptr[1 + 2 * 0] = -b * invdet
		A.ptr[1 + 2 * 1] = a * invdet
		return A
	end

	local lapacke = require 'ffi.req' 'lapacke'
	local getrfName = getLapackeNameForType(A.ctype, 'getrf')
	lapacke[getrfName](
		A.rowmajor and lapacke.LAPACK_ROW_MAJOR or lapacke.LAPACK_COL_MAJOR,	-- int matrix_layout,
		m,							-- lapack_int m,
		n, 							-- lapack_int n,
		A.ptr,						-- float* a,
		m,							-- int lda,
		ipiv.ptr) 					-- lapack_int* ipiv
	local getriName = getLapackeNameForType(A.ctype, 'getri')
	lapacke[getriName](
		A.rowmajor and lapacke.LAPACK_ROW_MAJOR or lapacke.LAPACK_COL_MAJOR,	-- int matrix_layout,
		n,							-- lapack_int n,
		A.ptr,						-- float* a,
		n,							-- int lda,
		ipiv.ptr)					-- const lapack_int* ipiv
	return A
end

-- https://www.mathworks.com/help/matlab/ref/expm.html
function matrix_ffi.expm(A)
	local D, VR, VL, beta, Di = matrix_ffi(A):eig()
	--local isComplex = tostring(A.ctype):find'complex'
	if not Di then
		assert(complex)	-- complex should exist if A.ctype is a complex type
		D = D:map(complex.exp)
	else
		assert(Di)
		D = D:map(math.exp):emul(Di:map(math.cos))
	end
	return VR * D:diag() * VR:inv()
end

matrix_ffi.determinant = require 'matrix.determinant'
matrix_ffi.det = matrix_ffi.determinant

function matrix_ffi:copy(src)
	assert.eq(self.size_, src.size_)
	ffi.copy(self.ptr, src.ptr, ffi.sizeof(self.ctype) * self.volume)
	return self
end

function matrix_ffi:clone()
	return matrix_ffi(self, self.ctype, self.size, self.rowmajor)
end

-- glsl functions:

local ident = matrix_ffi({
	{1,0,0,0},
	{0,1,0,0},
	{0,0,1,0},
	{0,0,0,1},
}, float)

-- optimized ... default mul of arbitrary-rank inner-product is verrrry slow
function matrix_ffi:mul4x4(a,b)
--DEBUG:assert.eq(#self.size_, 2)
--DEBUG:assert.eq(self.size_[1], 4)
--DEBUG:assert.eq(self.size_[2], 4)
--[[ no temp vars ... any perf diff?
	-- also assert self isn't the table a or b, or else this will mess up
	self.ptr[0] = a.ptr[0] * b.ptr[0] + a.ptr[4] * b.ptr[1] + a.ptr[8] * b.ptr[2] + a.ptr[12] * b.ptr[3]
	self.ptr[4] = a.ptr[0] * b.ptr[4] + a.ptr[4] * b.ptr[5] + a.ptr[8] * b.ptr[6] + a.ptr[12] * b.ptr[7]
	self.ptr[8] = a.ptr[0] * b.ptr[8] + a.ptr[4] * b.ptr[9] + a.ptr[8] * b.ptr[10] + a.ptr[12] * b.ptr[11]
	self.ptr[12] = a.ptr[0] * b.ptr[12] + a.ptr[4] * b.ptr[13] + a.ptr[8] * b.ptr[14] + a.ptr[12] * b.ptr[15]
	self.ptr[1] = a.ptr[1] * b.ptr[0] + a.ptr[5] * b.ptr[1] + a.ptr[9] * b.ptr[2] + a.ptr[13] * b.ptr[3]
	self.ptr[5] = a.ptr[1] * b.ptr[4] + a.ptr[5] * b.ptr[5] + a.ptr[9] * b.ptr[6] + a.ptr[13] * b.ptr[7]
	self.ptr[9] = a.ptr[1] * b.ptr[8] + a.ptr[5] * b.ptr[9] + a.ptr[9] * b.ptr[10] + a.ptr[13] * b.ptr[11]
	self.ptr[13] = a.ptr[1] * b.ptr[12] + a.ptr[5] * b.ptr[13] + a.ptr[9] * b.ptr[14] + a.ptr[13] * b.ptr[15]
	self.ptr[2] = a.ptr[2] * b.ptr[0] + a.ptr[6] * b.ptr[1] + a.ptr[10] * b.ptr[2] + a.ptr[14] * b.ptr[3]
	self.ptr[6] = a.ptr[2] * b.ptr[4] + a.ptr[6] * b.ptr[5] + a.ptr[10] * b.ptr[6] + a.ptr[14] * b.ptr[7]
	self.ptr[10] = a.ptr[2] * b.ptr[8] + a.ptr[6] * b.ptr[9] + a.ptr[10] * b.ptr[10] + a.ptr[14] * b.ptr[11]
	self.ptr[14] = a.ptr[2] * b.ptr[12] + a.ptr[6] * b.ptr[13] + a.ptr[10] * b.ptr[14] + a.ptr[14] * b.ptr[15]
	self.ptr[3] = a.ptr[3] * b.ptr[0] + a.ptr[7] * b.ptr[1] + a.ptr[11] * b.ptr[2] + a.ptr[15] * b.ptr[3]
	self.ptr[7] = a.ptr[3] * b.ptr[4] + a.ptr[7] * b.ptr[5] + a.ptr[11] * b.ptr[6] + a.ptr[15] * b.ptr[7]
	self.ptr[11] = a.ptr[3] * b.ptr[8] + a.ptr[7] * b.ptr[9] + a.ptr[11] * b.ptr[10] + a.ptr[15] * b.ptr[11]
	self.ptr[15] = a.ptr[3] * b.ptr[12] + a.ptr[7] * b.ptr[13] + a.ptr[11] * b.ptr[14] + a.ptr[15] * b.ptr[15]
--]]
-- [[
	local a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15 = a.ptr[0], a.ptr[1], a.ptr[2], a.ptr[3], a.ptr[4], a.ptr[5], a.ptr[6], a.ptr[7], a.ptr[8], a.ptr[9], a.ptr[10], a.ptr[11], a.ptr[12], a.ptr[13], a.ptr[14], a.ptr[15]
	local b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15 = b.ptr[0], b.ptr[1], b.ptr[2], b.ptr[3], b.ptr[4], b.ptr[5], b.ptr[6], b.ptr[7], b.ptr[8], b.ptr[9], b.ptr[10], b.ptr[11], b.ptr[12], b.ptr[13], b.ptr[14], b.ptr[15]
	self.ptr[0]		=	a0  * b0  + a4  * b1  + a8  * b2  + a12 * b3
	self.ptr[4]		=	a0  * b4  + a4  * b5  + a8  * b6  + a12 * b7
	self.ptr[8]		=	a0  * b8  + a4  * b9  + a8  * b10 + a12 * b11
	self.ptr[12]	=	a0  * b12 + a4  * b13 + a8  * b14 + a12 * b15
	self.ptr[1]		=	a1  * b0  + a5  * b1  + a9  * b2  + a13 * b3
	self.ptr[5]		=	a1  * b4  + a5  * b5  + a9  * b6  + a13 * b7
	self.ptr[9]		=	a1  * b8  + a5  * b9  + a9  * b10 + a13 * b11
	self.ptr[13]	=	a1  * b12 + a5  * b13 + a9  * b14 + a13 * b15
	self.ptr[2]		=	a2  * b0  + a6  * b1  + a10 * b2  + a14 * b3
	self.ptr[6]		=	a2  * b4  + a6  * b5  + a10 * b6  + a14 * b7
	self.ptr[10]	=	a2  * b8  + a6  * b9  + a10 * b10 + a14 * b11
	self.ptr[14]	=	a2  * b12 + a6  * b13 + a10 * b14 + a14 * b15
	self.ptr[3]		=	a3  * b0  + a7  * b1  + a11 * b2  + a15 * b3
	self.ptr[7]		=	a3  * b4  + a7  * b5  + a11 * b6  + a15 * b7
	self.ptr[11]	=	a3  * b8  + a7  * b9  + a11 * b10 + a15 * b11
	self.ptr[15]	=	a3  * b12 + a7  * b13 + a11 * b14 + a15 * b15
--]]
	return self
end

-- another optimized mul - this for vectors
function matrix_ffi:mul4x4v4(x,y,z,w)
	w = w or 1
	return
		self.ptr[0] * x + self.ptr[4] * y + self.ptr[8] * z + self.ptr[12] * w,
		self.ptr[1] * x + self.ptr[5] * y + self.ptr[9] * z + self.ptr[13] * w,
		self.ptr[2] * x + self.ptr[6] * y + self.ptr[10] * z + self.ptr[14] * w,
		self.ptr[3] * x + self.ptr[7] * y + self.ptr[11] * z + self.ptr[15] * w
end

function matrix_ffi:setIdent()
	if self.ctype == float then
		return self:copy(ident)
	end
	self.ptr[0],  self.ptr[1],  self.ptr[2],  self.ptr[3]  = 1, 0, 0, 0
	self.ptr[4],  self.ptr[5],  self.ptr[6],  self.ptr[7]  = 0, 1, 0, 0
	self.ptr[8],  self.ptr[9],  self.ptr[10], self.ptr[11] = 0, 0, 1, 0
	self.ptr[12], self.ptr[13], self.ptr[14], self.ptr[15] = 0, 0, 0, 1
	return self
end
function matrix_ffi:setOrtho(l,r,b,t,n,f)
--DEBUG:assert.eq(#self.size_, 2)
--DEBUG:assert.eq(self.size_[1], 4)
--DEBUG:assert.eq(self.size_[2], 4)
	n = n or -1000
	f = f or 1000
	local invdx = 1 / (r - l)
	local invdy = 1 / (t - b)
	local invdz = 1 / (f - n)
	self.ptr[0] = 2 * invdx
	self.ptr[4] = 0
	self.ptr[8] = 0
	self.ptr[12] = -(r + l) * invdx
	self.ptr[1] = 0
	self.ptr[5] = 2 * invdy
	self.ptr[9] = 0
	self.ptr[13] = -(t + b) * invdy
	self.ptr[2] = 0
	self.ptr[6] = 0
	self.ptr[10] = -2 * invdz
	self.ptr[14] = -(f + n) * invdz
	self.ptr[3] = 0
	self.ptr[7] = 0
	self.ptr[11] = 0
	self.ptr[15] = 1
	return self
end
function matrix_ffi:applyOrtho(l,r,b,t,n,f)
--DEBUG:assert.eq(#self.size_, 2)
--DEBUG:assert.eq(self.size_[1], 4)
--DEBUG:assert.eq(self.size_[2], 4)
	n = n or -1000
	f = f or 1000
	local invdx = 1 / (r - l)
	local invdy = 1 / (t - b)
	local invdz = 1 / (f - n)
	local rhs00 = 2 * invdx
	local rhs03 = -(r + l) * invdx
	local rhs11 = 2 * invdy
	local rhs13 = -(t + b) * invdy
	local rhs22 = -2 * invdz
	local rhs23 = -(f + n) * invdz
	local n00 = self.ptr[0] * rhs00
	local n01 = self.ptr[4] * rhs11
	local n02 = self.ptr[8] * rhs22
	local n03 = self.ptr[0] * rhs03 + self.ptr[4] * rhs13 + self.ptr[8] * rhs23 + self.ptr[12]
	local n10 = self.ptr[1] * rhs00
	local n11 = self.ptr[5] * rhs11
	local n12 = self.ptr[9] * rhs22
	local n13 = self.ptr[1] * rhs03 + self.ptr[5] * rhs13 + self.ptr[9] * rhs23 + self.ptr[13]
	local n20 = self.ptr[2] * rhs00
	local n21 = self.ptr[6] * rhs11
	local n22 = self.ptr[10] * rhs22
	local n23 = self.ptr[2] * rhs03 + self.ptr[6] * rhs13 + self.ptr[10] * rhs23 + self.ptr[14]
	local n30 = self.ptr[3] * rhs00
	local n31 = self.ptr[7] * rhs11
	local n32 = self.ptr[11] * rhs22
	local n33 = self.ptr[3] * rhs03 + self.ptr[7] * rhs13 + self.ptr[11] * rhs23 + self.ptr[15]
	self.ptr[0] = n00
	self.ptr[4] = n01
	self.ptr[8] = n02
	self.ptr[12] = n03
	self.ptr[1] = n10
	self.ptr[5] = n11
	self.ptr[9] = n12
	self.ptr[13] = n13
	self.ptr[2] = n20
	self.ptr[6] = n21
	self.ptr[10] = n22
	self.ptr[14] = n23
	self.ptr[3] = n30
	self.ptr[7] = n31
	self.ptr[11] = n32
	self.ptr[15] = n33
end

function matrix_ffi:setFrustum(l,r,b,t,n,f)
--DEBUG:assert.eq(#self.size_, 2)
--DEBUG:assert.eq(self.size_[1], 4)
--DEBUG:assert.eq(self.size_[2], 4)
--DEBUG:assert(not self.rowmajor)
	n = n or .1
	f = f or 1000
	local invdx = 1 / (r - l)
	local invdy = 1 / (t - b)
	local invdz = 1 / (f - n)
	self.ptr[0] = 2 * n * invdx
	self.ptr[4] = 0
	self.ptr[8] = (r + l) * invdx
	self.ptr[12] = 0
	self.ptr[1] = 0
	self.ptr[5] = 2 * n * invdy
	self.ptr[9] = (t + b) * invdy
	self.ptr[13] = 0
	self.ptr[2] = 0
	self.ptr[6] = 0
	self.ptr[10] = -(f + n) * invdz
	self.ptr[14] = -2 * f * n * invdz
	self.ptr[3] = 0
	self.ptr[7] = 0
	self.ptr[11] = -1
	self.ptr[15] = 0
	return self
end
function matrix_ffi:applyFrustum(l,r,b,t,n,f)
--DEBUG:assert.eq(#self.size_, 2)
--DEBUG:assert.eq(self.size_[1], 4)
--DEBUG:assert.eq(self.size_[2], 4)
--DEBUG:assert(not self.rowmajor)
	n = n or .1
	f = f or 1000
	local invdx = 1 / (r - l)
	local invdy = 1 / (t - b)
	local invdz = 1 / (f - n)

	local rhs0 = 2 * n * invdx
	local rhs8 = (r + l) * invdx
	local rhs5 = 2 * n * invdy
	local rhs9 = (t + b) * invdy
	local rhs10 = -(f + n) * invdz
	local rhs14 = -2 * f * n * invdz

	local new0 = self.ptr[0] * rhs0
	local new4 = self.ptr[4] * rhs5
	local new8 = self.ptr[0] * rhs8 + self.ptr[4] * rhs9 + self.ptr[8] * rhs10 - self.ptr[12]
	local new12 = self.ptr[8] * rhs14
	local new1 = self.ptr[1] * rhs0
	local new5 = self.ptr[5] * rhs5
	local new9 = self.ptr[1] * rhs8 + self.ptr[5] * rhs9 + self.ptr[9] * rhs10 - self.ptr[13]
	local new13 = self.ptr[9] * rhs14
	local new2 = self.ptr[2] * rhs0
	local new6 = self.ptr[6] * rhs5
	local new10 = self.ptr[2] * rhs8 + self.ptr[6] * rhs9 + self.ptr[10] * rhs10 - self.ptr[14]
	local new14 = self.ptr[10] * rhs14
	local new3 = self.ptr[3] * rhs0
	local new7 = self.ptr[7] * rhs5
	local new11 = self.ptr[3] * rhs8 + self.ptr[7] * rhs9 + self.ptr[11] * rhs10 - self.ptr[15]
	local new15 = self.ptr[11] * rhs14

	self.ptr[0] = new0
	self.ptr[4] = new4
	self.ptr[8] = new8
	self.ptr[12] = new12
	self.ptr[1] = new1
	self.ptr[5] = new5
	self.ptr[9] = new9
	self.ptr[13] = new13
	self.ptr[2] = new2
	self.ptr[6] = new6
	self.ptr[10] = new10
	self.ptr[14] = new14
	self.ptr[3] = new3
	self.ptr[7] = new7
	self.ptr[11] = new11
	self.ptr[15] = new15
end

-- http://iphonedevelopment.blogspot.com/2008/12/glulookat.html?m=1
local function cross(ax,ay,az,bx,by,bz)
	local cx = ay * bz - az * by
	local cy = az * bx - ax * bz
	local cz = ax * by - ay * bx
	return cx,cy,cz
end
local function normalize(x,y,z)
	local m = math.sqrt(x*x + y*y + z*z)
	if m > 1e-20 then
		return x/m, y/m, z/m
	end
	return 1,0,0
end
-- https://www.khronos.org/opengl/wiki/GluLookAt_code
-- ex ey ez is where the view is centered (lol not 'center')
-- cx cy cz is where the view is looking at
-- upx upy upz is the up vector
function matrix_ffi:setLookAt(ex,ey,ez,cx,cy,cz,upx,upy,upz)
--DEBUG:assert.eq(#self.size_, 2)
--DEBUG:assert.eq(self.size_[1], 4)
--DEBUG:assert.eq(self.size_[2], 4)
--DEBUG:assert(not self.rowmajor)
	local forwardx, forwardy, forwardz = normalize(cx-ex, cy-ey, cz-ez)
	local sidex, sidey, sidez = normalize(cross(forwardx, forwardy, forwardz, upx, upy, upz))
	upx, upy, upz = normalize(cross(sidex, sidey, sidez, forwardx, forwardy, forwardz))
	self.ptr[0] = sidex
	self.ptr[4] = sidey
	self.ptr[8] = sidez
	self.ptr[12] = 0
	self.ptr[1] = upx
	self.ptr[5] = upy
	self.ptr[9] = upz
	self.ptr[13] = 0
	self.ptr[2] = -forwardx
	self.ptr[6] = -forwardy
	self.ptr[10] = -forwardz
	self.ptr[14] = 0
	self.ptr[3] = 0
	self.ptr[7] = 0
	self.ptr[11] = 0
	self.ptr[15] = 1
	return self:applyTranslate(-ex, -ey, -ez)
end
-- TODO optimize the in-place apply instead of this slow crap:
local tmp = matrix_ffi{4,4}:zeros()
function matrix_ffi:applyLookAt(...)
	return self:mul4x4(self, tmp:setLookAt(...))
end

-- axis is expected to be unit
function matrix_ffi:setRotateCosSinUnit(c, s, x, y, z)
--DEBUG:assert.eq(#self.size_, 2)
--DEBUG:assert.eq(self.size_[1], 4)
--DEBUG:assert.eq(self.size_[2], 4)
--DEBUG:assert(not self.rowmajor)
	local ic = 1 - c
	self.ptr[0] = c + x*x*ic
	self.ptr[4] = x*y*ic - z*s
	self.ptr[8] = x*z*ic + y*s
	self.ptr[12] = 0
	self.ptr[1] = x*y*ic + z*s
	self.ptr[5] = c + y*y*ic
	self.ptr[9] = y*z*ic - x*s
	self.ptr[13] = 0
	self.ptr[2] = x*z*ic - y*s
	self.ptr[6] = y*z*ic + x*s
	self.ptr[10] = c + z*z*ic
	self.ptr[14] = 0
	self.ptr[3] = 0
	self.ptr[7] = 0
	self.ptr[11] = 0
	self.ptr[15] = 1
	return self
end
function matrix_ffi:applyRotateCosSinUnit(c, s, x, y, z)
--DEBUG:assert.eq(#self.size_, 2)
--DEBUG:assert.eq(self.size_[1], 4)
--DEBUG:assert.eq(self.size_[2], 4)
--DEBUG:assert(not self.rowmajor)
	local ic = 1 - c
	local a0 = self.ptr[0]
	local a1 = self.ptr[1]
	local a2 = self.ptr[2]
	local a3 = self.ptr[3]
	local a4 = self.ptr[4]
	local a5 = self.ptr[5]
	local a6 = self.ptr[6]
	local a7 = self.ptr[7]
	local a8 = self.ptr[8]
	local a9 = self.ptr[9]
	local a10 = self.ptr[10]
	local a11 = self.ptr[11]

	local b0 = c + x*x*ic
	local b4 = x*y*ic - z*s
	local b8 = x*z*ic + y*s
	local b1 = x*y*ic + z*s
	local b5 = c + y*y*ic
	local b9 = y*z*ic - x*s
	local b2 = x*z*ic - y*s
	local b6 = y*z*ic + x*s
	local b10 = c + z*z*ic

	self.ptr[0] = a0 * b0 + a4 * b1 + a8 * b2
	self.ptr[1] = a1 * b0 + a5 * b1 + a9 * b2
	self.ptr[2] = a2 * b0 + a6 * b1 + a10 * b2
	self.ptr[3] = a3 * b0 + a7 * b1 + a11 * b2
	self.ptr[4] = a0 * b4 + a4 * b5 + a8 * b6
	self.ptr[5] = a1 * b4 + a5 * b5 + a9 * b6
	self.ptr[6] = a2 * b4 + a6 * b5 + a10 * b6
	self.ptr[7] = a3 * b4 + a7 * b5 + a11 * b6
	self.ptr[8] = a0 * b8 + a4 * b9 + a8 * b10
	self.ptr[9] = a1 * b8 + a5 * b9 + a9 * b10
	self.ptr[10] = a2 * b8 + a6 * b9 + a10 * b10
	self.ptr[11] = a3 * b8 + a7 * b9 + a11 * b10

	return self
end

-- axis is optional
-- if axis is not provided or if it is near-zero length, defaults to 0,0,1
function matrix_ffi:setRotateCosSin(c, s, x, y, z)
	if not x then x,y,z = 0,0,1 end
--DEBUG:assert.eq(#self.size_, 2)
--DEBUG:assert.eq(self.size_[1], 4)
--DEBUG:assert.eq(self.size_[2], 4)
--DEBUG:assert(not self.rowmajor)
	local l = math.sqrt(x*x + y*y + z*z)
	if l < 1e-20 then
		x=1
		y=0
		z=0
	else
		local il = 1/l
		x=x*il
		y=y*il
		z=z*il
	end
	return self:setRotateCosSinUnit(c, s, x, y, z)
end
function matrix_ffi:applyRotateCosSin(c, s, x, y, z)
--DEBUG:assert.eq(#self.size_, 2)
--DEBUG:assert.eq(self.size_[1], 4)
--DEBUG:assert.eq(self.size_[2], 4)
--DEBUG:assert(not self.rowmajor)
	if not x then x,y,z = 0,0,1 end
	local l = math.sqrt(x*x + y*y + z*z)
	if l < 1e-20 then
		x=1
		y=0
		z=0
	else
		local il = 1/l
		x=x*il
		y=y*il
		z=z*il
	end
	return self:applyRotateCosSinUnit(c, s, x, y, z)
end

function matrix_ffi:setRotate(radians, ...)
	return self:setRotateCosSin(math.cos(radians), math.sin(radians), ...)
end
function matrix_ffi:applyRotate(radians, ...)
	return self:applyRotateCosSin(math.cos(radians), math.sin(radians), ...)
end

function matrix_ffi:setScale(x,y,z)
	x = x or 1
	y = y or 1
	z = z or 1
--DEBUG:assert.eq(#self.size_, 2)
--DEBUG:assert.eq(self.size_[1], 4)
--DEBUG:assert.eq(self.size_[2], 4)
--DEBUG:assert(not self.rowmajor)
	self.ptr[0] = x
	self.ptr[1] = 0
	self.ptr[2] = 0
	self.ptr[3] = 0
	self.ptr[4] = 0
	self.ptr[5] = y
	self.ptr[6] = 0
	self.ptr[7] = 0
	self.ptr[8] = 0
	self.ptr[9] = 0
	self.ptr[10] = z
	self.ptr[11] = 0
	self.ptr[12] = 0
	self.ptr[13] = 0
	self.ptr[14] = 0
	self.ptr[15] = 1
	return self
end
function matrix_ffi:applyScale(x,y,z)
--DEBUG:assert.eq(#self.size_, 2)
--DEBUG:assert.eq(self.size_[1], 4)
--DEBUG:assert.eq(self.size_[2], 4)
--DEBUG:assert(not self.rowmajor)
	if x then
		self.ptr[0] = self.ptr[0] * x
		self.ptr[1] = self.ptr[1] * x
		self.ptr[2] = self.ptr[2] * x
		self.ptr[3] = self.ptr[3] * x
	end
	if y then
		self.ptr[4] = self.ptr[4] * y
		self.ptr[5] = self.ptr[5] * y
		self.ptr[6] = self.ptr[6] * y
		self.ptr[7] = self.ptr[7] * y
	end
	if z then
		self.ptr[8] = self.ptr[8] * z
		self.ptr[9] = self.ptr[9] * z
		self.ptr[10] = self.ptr[10] * z
		self.ptr[11] = self.ptr[11] * z
	end
	return self
end

function matrix_ffi:setTranslate(x,y,z)
	x = x or 0
	y = y or 0
	z = z or 0
--DEBUG:assert.eq(#self.size_, 2)
--DEBUG:assert.eq(self.size_[1], 4)
--DEBUG:assert.eq(self.size_[2], 4)
--DEBUG:assert(not self.rowmajor)
	self.ptr[0] = 1
	self.ptr[1] = 0
	self.ptr[2] = 0
	self.ptr[3] = 0
	self.ptr[4] = 0
	self.ptr[5] = 1
	self.ptr[6] = 0
	self.ptr[7] = 0
	self.ptr[8] = 0
	self.ptr[9] = 0
	self.ptr[10] = 1
	self.ptr[11] = 0
	self.ptr[12] = x
	self.ptr[13] = y
	self.ptr[14] = z
	self.ptr[15] = 1
	return self
end
function matrix_ffi:applyTranslate(x,y,z)
	x = x or 0
	y = y or 0
	z = z or 0
--DEBUG:assert.eq(#self.size_, 2)
--DEBUG:assert.eq(self.size_[1], 4)
--DEBUG:assert.eq(self.size_[2], 4)
--DEBUG:assert(not self.rowmajor)
	self.ptr[12] = x * self.ptr[0] + y * self.ptr[4] + z * self.ptr[8] + self.ptr[12]
	self.ptr[13] = x * self.ptr[1] + y * self.ptr[5] + z * self.ptr[9] + self.ptr[13]
	self.ptr[14] = x * self.ptr[2] + y * self.ptr[6] + z * self.ptr[10] + self.ptr[14]
	self.ptr[15] = x * self.ptr[3] + y * self.ptr[7] + z * self.ptr[11] + self.ptr[15]
	return self
end

-- based on the mesa impl: https://community.khronos.org/t/glupickmatrix-implementation/72008/2
-- except that I'm going to assume x, y, dx, dy are normalized to [0,1] instead of [0,viewport-1] so that you don't have to also get and pass the viewport
function matrix_ffi:setPickMatrix(...)
--DEBUG:assert.eq(#self.size_, 2)
--DEBUG:assert.eq(self.size_[1], 4)
--DEBUG:assert.eq(self.size_[2], 4)
	return self:setIdent():applyPickMatrix(...)
end
function matrix_ffi:applyPickMatrix(x, y, dx, dy)
--DEBUG:assert.eq(#self.size_, 2)
--DEBUG:assert.eq(self.size_[1], 4)
--DEBUG:assert.eq(self.size_[2], 4)
	if dx <= 0 or dy <= 0 then return self end
	return self
		:applyTranslate(
			(1 - 2 * x) / dx,
			(1 - 2 * y) / dy,
			0)
		:applyScale(
			1 / dx,
			1 / dy,
			1)
end

-- based on mesa: https://github.com/Starlink/mesa/blob/master/src/glu/sgi/libutil/project.c
function matrix_ffi:setPerspective(fovy, aspectRatio, zNear, zFar)
--DEBUG:assert.eq(#self.size_, 2)
--DEBUG:assert.eq(self.size_[1], 4)
--DEBUG:assert.eq(self.size_[2], 4)
	local radians = math.rad(.5 * fovy)
	local deltaZ = zFar - zNear
	local sine = math.sin(radians)
	if deltaZ == 0 or sine == 0 or aspectRatio == 0 then return self end
	local cotangent = math.cos(radians) / sine
	self:setIdent()
	self.ptr[0 + 4 * 0] = cotangent / aspectRatio
	self.ptr[1 + 4 * 1] = cotangent
	self.ptr[2 + 4 * 2] = -(zFar + zNear) / deltaZ
	self.ptr[2 + 4 * 3] = -1
	self.ptr[3 + 4 * 2] = -2 * zNear * zFar / deltaZ
	self.ptr[3 + 4 * 3] = 0
	return self
end
function matrix_ffi:applyPerspective(...)
	return self:mul4x4(
		self,
		matrix_ffi({4, 4}, float):zeros():setPerspective(...)
	)
end

-- calculates the inverse of 'src' or 'self' and stores it in 'self'
-- https://stackoverflow.com/a/1148405
function matrix_ffi:inv4x4(src)
--DEBUG:assert.eq(#self.size_, 2)
--DEBUG:assert.eq(self.size_[1], 4)
--DEBUG:assert.eq(self.size_[2], 4)
--DEBUG:assert(not self.rowmajor)
	src = src or self
	local srcp = src.ptr
	local a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15
		= srcp[0], srcp[1], srcp[2], srcp[3], srcp[4], srcp[5], srcp[6], srcp[7], srcp[8], srcp[9], srcp[10], srcp[11], srcp[12], srcp[13], srcp[14], srcp[15]

	local dstp = self.ptr
	dstp[0]  =  a5 * a10 * a15 - a5 * a11 * a14 - a9 * a6 * a15 + a9 * a7 * a14 + a13 * a6 * a11 - a13 * a7 * a10
	dstp[1]  = -a1 * a10 * a15 + a1 * a11 * a14 + a9 * a2 * a15 - a9 * a3 * a14 - a13 * a2 * a11 + a13 * a3 * a10
	dstp[2]  =  a1 * a6 * a15 - a1 * a7 * a14 - a5 * a2 * a15 + a5 * a3 * a14 + a13 * a2 * a7 - a13 * a3 * a6
	dstp[3]  = -a1 * a6 * a11 + a1 * a7 * a10 + a5 * a2 * a11 - a5 * a3 * a10 - a9 * a2 * a7 + a9 * a3 * a6
	dstp[4]  = -a4 * a10 * a15 + a4 * a11 * a14 + a8 * a6 * a15 - a8 * a7 * a14 - a12 * a6 * a11 + a12 * a7 * a10
	dstp[5]  =  a0 * a10 * a15 - a0 * a11 * a14 - a8 * a2 * a15 + a8 * a3 * a14 + a12 * a2 * a11 - a12 * a3 * a10
	dstp[6]  = -a0 * a6 * a15 + a0 * a7 * a14 + a4 * a2 * a15 - a4 * a3 * a14 - a12 * a2 * a7 + a12 * a3 * a6
	dstp[7]  =  a0 * a6 * a11 - a0 * a7 * a10 - a4 * a2 * a11 + a4 * a3 * a10 + a8 * a2 * a7 - a8 * a3 * a6
	dstp[8]  =  a4 * a9 * a15 - a4 * a11 * a13 - a8 * a5 * a15 + a8 * a7 * a13 + a12 * a5 * a11 - a12 * a7 * a9
	dstp[9]  = -a0 * a9 * a15 + a0 * a11 * a13 + a8 * a1 * a15 - a8 * a3 * a13 - a12 * a1 * a11 + a12 * a3 * a9
	dstp[10] =  a0 * a5 * a15 - a0 * a7 * a13 - a4 * a1 * a15 + a4 * a3 * a13 + a12 * a1 * a7 - a12 * a3 * a5
	dstp[11] = -a0 * a5 * a11 + a0 * a7 * a9 + a4 * a1 * a11 - a4 * a3 * a9 - a8 * a1 * a7 + a8 * a3 * a5
	dstp[12] = -a4 * a9 * a14 + a4 * a10 * a13 + a8 * a5 * a14 - a8 * a6 * a13 - a12 * a5 * a10 + a12 * a6 * a9
	dstp[13] =  a0 * a9 * a14 - a0 * a10 * a13 - a8 * a1 * a14 + a8 * a2 * a13 + a12 * a1 * a10 - a12 * a2 * a9
	dstp[14] = -a0 * a5 * a14 + a0 * a6 * a13 + a4 * a1 * a14 - a4 * a2 * a13 - a12 * a1 * a6 + a12 * a2 * a5
	dstp[15] =  a0 * a5 * a10 - a0 * a6 * a9 - a4 * a1 * a10 + a4 * a2 * a9 + a8 * a1 * a6 - a8 * a2 * a5

	local det = a0 * dstp[0] + a1 * dstp[4] + a2 * dstp[8] + a3 * dstp[12]
	if det == 0 then
		-- if this is in-place then do we error or return an extra flag or something?
		for i=0,15 do
			dstp[i] = 0/0
		end
		return self, 'singular'
	end

	local invdet = 1 / det
	for i=0,15 do
		dstp[i] = dstp[i] * invdet
	end

	return self
end

return matrix_ffi
