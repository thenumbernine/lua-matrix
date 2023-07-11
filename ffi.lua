-- my attempt at a ffi implementation of the matrix class
-- take note, this relies on luajit
-- and luajit doesn't overload __len
local ffi = require 'ffi'
local class = require 'ext.class'
local table = require 'ext.table'

local complex
local function requireComplex()
	complex = require 'complex'
	return complex
end

-- here's my original pure-Lua version
local matrix_lua = require 'matrix'

local matrix_ffi = class()

-- override this to specify a default for the ctor ctype parameter
matrix_ffi.real = nil

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
function matrix_ffi:init(src, ctype, size)
	if type(src) == 'table'
	and not matrix_ffi:isa(src)
	then
		src = matrix_lua(src)
	end
	-- make sure src is a matrix_ffi or matrix_lua

	-- the size member can't be a matrix_ffi or we'll get infinite recursion ...
	-- so I could make it a regular table
	-- but why not a matrix_lua, so we get some similar functionality?
	if matrix_ffi:isa(src) then
		self.size_ = matrix_lua{src:size():unpack()}
	elseif matrix_lua:isa(src) then
		self.size_ = src:size()
	elseif size then
		if matrix_ffi:isa(size) then
			self.size_ = matrix_lua{size:unpack()}
		elseif type(size) == 'table' then
			self.size_ = matrix_lua{table.unpack(size)}
		else
			self.size_ = matrix_lua{size}
		end
	else
		assert(src == nil)
		self.size_ = matrix_lua{0}
	end

	self.volume = self.size_:prod()

	self.step = matrix_lua(self.size_)
	-- TODO make step optional row vs col major
	self.step[1] = 1
	for i=2,#self.size_ do
		self.step[i] = self.step[i-1] * self.size_[i-1]
	end

	self.ctype = ctype 			-- ctype arg
		or (src and src.ctype) 	-- src ctype
		or self.real 			-- default ctype
		or 'double'				-- last case

	-- if we're building a matrix with complex ctype ...
	if self.ctype:lower():find'complex' then
		-- then make sure complex ctypes have their metamethods defined
		requireComplex()
	end
	-- use only one name for when multiple work
	-- idk how i'll handle typedefs
	-- is there a luajit ffi api for looking up typedef original types?
	if self.ctype == 'complex' then self.ctype = 'complex double' end

	self.ptr = ffi.new(self.ctype..'[?]', math.max(self.volume,1))

	if matrix_ffi:isa(src) then
		if src.type == self.ctype then
			ffi.copy(self.ptr, src.ptr, ffi.sizeof(self.ctype) * self.volume)
		else
			local mn = math.min(self.volume, src.volume)
			for i=0,mn-1 do
				self.ptr[i] = src.ptr[i]
			end
			if mn < self.volume then
				local zero = ffi.new(self.ctype)
				for i=mn,self.volume-1 do
					self.ptr[i] = zero
				end
			end
		end
	elseif src ~= nil then
		for i in src:iter() do
			self[i] = src[i]
		end
	end
end

function matrix_ffi.cast(A, ctype)
	return matrix_ffi(A, ctype)
end

-- sorry, for my matrix lib compat,
-- you gotta set ctypes with matrix_ffi.real = whatever ctype
function matrix_ffi.const(value, dims, ctype)
	ctype = ctype or dims.ctype
	assert(type(dims) == 'table')
	if not (ctype == nil or type(ctype) == 'string') then
		error("got unknown ctype: "..require 'ext.tolua'(ctype))
	end
	local result = matrix_ffi(nil, ctype, matrix_ffi(dims))
	for i=0,result.volume-1 do
		result.ptr[i] = value
	end
	return result
end

-- matches matrix_lua except the matrix ref
function matrix_ffi.zeros(dims, ctype)
	ctype = ctype or dims.ctype
	assert(ctype == nil or type(ctype) == 'string')
	return matrix_ffi.const(0, dims, ctype)
end

-- matches matrix_lua except the matrix ref
function matrix_ffi.ones(dims, ctype)
	return matrix_ffi.const(1, dims, ctype)
end

-- could match matrix_lua except the matrix ref, if I copied it back over, but it might be slightly slower?
function matrix_ffi.lambda(size, f, result, ctype)
	size = matrix_ffi:isa(size)
		and size
		or matrix_ffi(size)
	ctype = ctype or size.ctype
	if not (ctype == nil or type(ctype) == 'string') then
		error("got unknown ctype: "..require 'ext.tolua'(ctype))
	end
	if size:degree() == 0 then return f() end
	if not result then result = size:zeros(ctype) end
	for i in result:iter() do
		local x = assert(f(i:unpack()))
		result[i] = x
	end
	return result
end

-- returns the matrix size
-- size_ is stored as a matrix_lua, but :size() will return a matrix_ffi
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
	local firstI = ...
	local n = select('#', ...)
	if type(firstI) == 'table' then error'TODO' end
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
		i = matrix_lua{i}
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
	if isnumber(b) then
		for i=0,c.volume-1 do
			c.ptr[i] = c.ptr[i] - b
		end
	elseif matrix_ffi:isa(b) then
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
	return self:size():range()
end


-- matches matrix_lua except matrix ref
function matrix_ffi.scale(a,s)
	if isnumber(a) then return a * s end
	assert(isnumber(s))
	a = matrix_ffi(a)
	for i in a:iter() do
		a[i] = a[i] * s
	end
	return a
end

-- could match matrix_lua except matrix ref if I copied it back
function matrix_ffi.inner(a,b,metric,aj,bj, c)
	if isnumber(a) then
		if isnumber(b) then return a * b end
		return b:scale(a)
	elseif isnumber(b) then
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
	assert(a:size() == b:size())
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
	assert(matrix_ffi:isa(a))
	assert(matrix_ffi:isa(b))
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

-- [[ copied from matrix.lua
function matrix_ffi:transpose(aj,bj)
	--dimensions to transpose
	aj = aj or 1
	bj = bj or 2
	local size = self:size()
	size[aj], size[bj] = size[bj], size[aj]
	return matrix_ffi.lambda(size, function(...)
		local si = {...}
		si[aj], si[bj] = si[bj], si[aj]
		return self[si]
	end, nil, self.ctype)
end

function matrix_ffi:T(...)
	return self:transpose(...)
end
--]]

function matrix_ffi:map(f)
	return self:size():lambda(function(...)
		return f(self(...), ...)
	end)
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
			return ffi.new(self.ctype)
		end
	end, nil, self.ctype)
end

function matrix_ffi.eye(size, ctype)
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
	end, nil, ctype)
end

------------ LAPACKE SUPPORT ------------
-- should I put this in its own file?
-- right now it only tries to load lapacke if any functions are called, so I dont think separating it into another file is necessary

-- NOTICE I'm not handling any type for 'complex' alone, which luajit has synonymous for 'complex double'
-- so maybe I'll change the matrix_ffi ctor to swap that out

local scalarTypeForComplexType = {
	float = 'float',
	double = 'double',
	['complex float'] = 'float',
	['complex double'] = 'double',
}

local lapackeLetterForType = {
	float = 's',
	double = 'd',
	['complex float'] = 'c',
	['complex double'] = 'z',
}

local function getLapackeNameForType(ctype, name)
	local letter = lapackeLetterForType[ctype]
	if not letter then
		error("can't find the lapacke letter associated with the ctype "..tostring(ctype))
	end
	return 'LAPACKE_'..letter..name
end


--[[
perform a 3x3 svd
for the sake of a 3x3 matrix-exponent
https://www.ibm.com/docs/en/essl/6.2?topic=llss-sgesvd-dgesvd-cgesvd-zgesvd-sgesdd-dgesdd-cgesdd-zgesdd-singular-value-decomposition-general-matrix
--]]
function matrix_ffi.svd(A)
	local lapacke = require 'ffi.lapacke'
	A = matrix_ffi(A)	-- don't modify original
--print('A.ctype', A.ctype)
	local size = A:size()
	assert(matrix_ffi:isa(size))
	local svdName = getLapackeNameForType(A.ctype, 'gesvd')
	local m, n = size:unpack()
	local U = matrix_ffi(nil, A.ctype, size)
--print('U.ctype', U.ctype)
	-- TODO or just remove the 'complex' from the type, if it is there
	local scalarType = scalarTypeForComplexType[A.ctype]
	if not scalarType then
		error("can't find scalar type for C type "..A.ctype)
	end
	local S = matrix_ffi(nil, scalarType, {m})
--print('S.ctype', S.ctype)
	local VT = matrix_ffi(nil, A.ctype, size)
--print('VT.ctype', VT.ctype)
	local superb = ffi.new(scalarType..'[2]') -- ... ???
	lapacke[svdName](
		lapacke.LAPACK_COL_MAJOR,	-- int matrix_layout,
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
	local lapacke = require 'ffi.lapacke'
	A = matrix_ffi(A)	-- don't modify original
	local eigName = getLapackeNameForType(A.ctype, 'ggev')
	local size = A:size()
	assert(matrix_ffi:isa(size))
	local m, n = size:unpack()
	assert(m == n)
	B = B or size:eye(A.ctype)
	local alpha = matrix_ffi(nil, A.ctype, {n})
	local beta = matrix_ffi(nil, A.ctype, {n})
	local VL = matrix_ffi(nil, A.ctype, size)
	local VR = matrix_ffi(nil, A.ctype, size)
	local alphai
	-- too bad, I was really hoping all lapack functions of matching suffixes has matching # of args
	if A.ctype == 'complex float'
	or A.ctype == 'complex double'
	then
--print("cplx path")
		lapacke[eigName](
			lapacke.LAPACK_COL_MAJOR,	-- int matrix_layout,
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
	elseif A.ctype == 'float'
	or A.ctype == 'double'
	then
--print("real path")
		alphai = matrix_ffi(nil, A.ctype, {n})
		lapacke[eigName](
			lapacke.LAPACK_COL_MAJOR,	-- int matrix_layout,
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
	assert(m == n)	-- needed for getri
	local mn = math.min(m,n)
	local ipiv = matrix_ffi(nil, 'int', {mn})

	-- ok lapacke is sometimes the ffi.cdef that 'table overflow's luajit
	-- meaning i'm including too much in luajit
	-- so lets avoid it if possible ...
	if m == 2 then
		local a,b,c,d = A[1][1], A[2][1], A[1][2], A[2][2]
		local det = a * d - b * c
		-- A_ij pos is i + m * j
		A.ptr[0 + 2 * 0] = d / det
		A.ptr[0 + 2 * 1] = -c / det
		A.ptr[1 + 2 * 0] = -b / det
		A.ptr[1 + 2 * 1] = a / det
		return A
	end

	local lapacke = require 'ffi.lapacke'
	local getrfName = getLapackeNameForType(A.ctype, 'getrf')
	lapacke[getrfName](
		lapacke.LAPACK_COL_MAJOR,	-- int matrix_layout,
		m,							-- lapack_int m,
		n, 							-- lapack_int n,
		A.ptr,						-- float* a,
		m,							-- int lda,
		ipiv.ptr) 					-- lapack_int* ipiv
	local getriName = getLapackeNameForType(A.ctype, 'getri')
	lapacke[getriName](
		lapacke.LAPACK_COL_MAJOR,	-- int matrix_layout,
		n,							-- lapack_int n,
		A.ptr,						-- float* a,
		n,							-- int lda,
		ipiv.ptr)					-- const lapack_int* ipiv
	return A
end

-- https://www.mathworks.com/help/matlab/ref/expm.html
function matrix_ffi.expm(A)
	local D, VR, VL, beta, Di = matrix_ffi(A):eig()
	--local isComplex = A.ctype:find'complex'
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
	assert(self:size() == src:size())
	ffi.copy(self.ptr, src.ptr, self.volume)
	return self
end

-- glsl functions:

local ident = matrix_ffi({4,4}, 'float'):lambda(function(i,j)
	return i==j and 1 or 0
end)


function matrix_ffi:setIdent()
	return self:copy(ident)
end
function matrix_ffi:setOrtho(l,r,b,t,n,f)
	assert(#self.size_ == 2 and self.size_[1] == 4 and self.size_[2] == 4)
	self.ptr[0] = 2 / (r - l)
	self.ptr[4] = 0
	self.ptr[8] = 0
	self.ptr[12] = -(r + l) / (r - l)
	self.ptr[1] = 0
	self.ptr[5] = 2 / (t - b)
	self.ptr[9] = 0
	self.ptr[13] = -(t + b) / (t - b)
	self.ptr[2] = 0
	self.ptr[6] = 0
	self.ptr[10] = -2 / (f - n)
	self.ptr[14] = -(f + n) / (f - n)
	self.ptr[3] = 0
	self.ptr[7] = 0
	self.ptr[11] = 0
	self.ptr[15] = 1
	return self
end
-- TODO optimize the in-place apply instead of this slow crap:
function matrix_ffi:applyOrtho(...)
	return self:copy(self * matrix_ffi{4,4}:zeros():setOrtho(...))
end

function matrix_ffi:setFrustum(l,r,b,t,n,f)
	assert(#self.size_ == 2 and self.size_[1] == 4 and self.size_[2] == 4)
	self.ptr[0] = 2 * n / (r - l)
	self.ptr[4] = 0
	self.ptr[8] = (r + l) / (r - l)
	self.ptr[12] = 0
	self.ptr[1] = 0
	self.ptr[5] = 2 * n / (t - b)
	self.ptr[9] = (t + b) / (t - b)
	self.ptr[13] = 0
	self.ptr[2] = 0
	self.ptr[6] = 0
	self.ptr[10] = -(f + n) / (f - n)
	self.ptr[14] = -2 * f * n / (f - n)
	self.ptr[3] = 0
	self.ptr[7] = 0
	self.ptr[11] = -1
	self.ptr[15] = 0
	return self
end
-- TODO optimize the in-place apply instead of this slow crap:
function matrix_ffi:applyFrustum(...)
	return self:copy(self * matrix_ffi{4,4}:zeros():setFrustum(...))
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
function matrix_ffi:setLookAt(ex,ey,ez,cx,cy,cz,upx,upy,upz)
	assert(#self.size_ == 2 and self.size_[1] == 4 and self.size_[2] == 4)
	local zx,zy,zz = normalize(ex-cx,ey-cy,ez-cz)
	local xx, xy, xz = normalize(cross(upx,upy,upz,zx,zy,zz))
	local yx, yy, yz = normalize(cross(zx,zy,zz,xx,xy,xz))
	self.ptr[0] = xx
	self.ptr[4] = xy
	self.ptr[8] = xz
	self.ptr[12] = 0
	self.ptr[1] = yx
	self.ptr[5] = yy
	self.ptr[9] = yz
	self.ptr[13] =0
	self.ptr[2] = zx
	self.ptr[6] = zy
	self.ptr[10] = zz
	self.ptr[14] = 0
	self.ptr[3] = 0
	self.ptr[7] = 0
	self.ptr[11] = 0
	self.ptr[15] = 1
	return self
end
-- TODO optimize the in-place apply instead of this slow crap:
function matrix_ffi:applyLookAt(...)
	return self:copy(self * matrix_ffi{4,4}:zeros():setLookAt(...))
end

function matrix_ffi:setRotate(degrees,x,y,z)
	assert(#self.size_ == 2 and self.size_[1] == 4 and self.size_[2] == 4)
	local r = math.rad(degrees)
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
	local c = math.cos(r)
	local s = math.sin(r)
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
-- TODO optimize the in-place apply instead of this slow crap:
function matrix_ffi:applyRotate(...)
	return self:copy(self * matrix_ffi{4,4}:zeros():setRotate(...))
end

function matrix_ffi:setScale(x,y,z)
	x = x or 1
	y = y or 1
	z = z or 1
	assert(#self.size_ == 2 and self.size_[1] == 4 and self.size_[2] == 4)
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
-- TODO optimize the in-place apply instead of this slow crap:
function matrix_ffi:applyScale(...)
	return self:copy(self * matrix_ffi{4,4}:zeros():setScale(...))
end

function matrix_ffi:setTranslate(x,y,z)
	x = x or 0
	y = y or 0
	z = z or 0
	assert(#self.size_ == 2 and self.size_[1] == 4 and self.size_[2] == 4)
	self.ptr[0] = 1
	self.ptr[4] = 0
	self.ptr[8] = 0
	self.ptr[12] = x
	self.ptr[1] = 0
	self.ptr[5] = 1
	self.ptr[9] = 0
	self.ptr[13] = y
	self.ptr[2] = 0
	self.ptr[6] = 0
	self.ptr[10] = 1
	self.ptr[14] = z
	self.ptr[3] = 0
	self.ptr[7] = 0
	self.ptr[11] = 0
	self.ptr[15] = 1
	return self
end
-- TODO optimize the in-place apply instead of this slow crap:
function matrix_ffi:applyTranslate(...)
	return self:copy(self * matrix_ffi{4,4}:zeros():setTranslate(...))
end

--[[ optimize the :apply and this isn't needed anymore right?
function matrix_ffi:translateMultScale(x,y,z,sx,sy,sz)
	assert(#self.size_ == 2 and self.size_[1] == 4 and self.size_[2] == 4)
	self.ptr[0] = sx
	self.ptr[4] = 0
	self.ptr[8] = 0
	self.ptr[12] = x
	self.ptr[1] = 0
	self.ptr[5] = sy
	self.ptr[9] = 0
	self.ptr[13] = y
	self.ptr[2] = 0
	self.ptr[6] = 0
	self.ptr[10] = sz
	self.ptr[14] = z
	self.ptr[3] = 0
	self.ptr[7] = 0
	self.ptr[11] = 0
	self.ptr[15] = 1
	return self
end
--]]

return matrix_ffi
