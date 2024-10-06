#!/usr/bin/env luajit
--[[
make sure all the :set, :apply, operator*, and :mul4x4 all work correctly ... for col and row majors?  :set and :apply only for col-majors, since they are supposed to be OpenGL replacement functions?

[a b 0 0] [c d 0 0]   [ac ad+b 0 0]
[0 1 0 0] [0 1 0 0]   [ 0   1  0 0]
[0 0 1 0] [0 0 1 0] = [ 0   0  1 0]
[0 0 0 1] [0 0 0 1]   [ 0   0  0 1]

[c d 0 0] [a b 0 0]   [ac bc+d 0 0]
[0 1 0 0] [0 1 0 0]   [ 0   1  0 0]
[0 0 1 0] [0 0 1 0] = [ 0   0  1 0]
[0 0 0 1] [0 0 0 1]   [ 0   0  0 1]


[a 0 0 0] [c 0 0 0]   [ac   0 0 0]
[b 1 0 0] [d 1 0 0]   [bc+d 1 0 0]
[0 0 1 0] [0 0 1 0] = [ 0   0 1 0]
[0 0 0 1] [0 0 0 1]   [ 0   0 0 1]

--]]
local table = require 'ext.table'
local range = require 'ext.range'
local asserteq = require 'ext.assert'.eq
local assertindex = require 'ext.assert'.index
local matrix = require 'matrix.ffi'

local function zero()
	return matrix{4,4}:zeros()
end
local function eye()
	return matrix{4,4}:eye()
end

local lambdaTest = matrix{4,4}:lambda(function(i,j)	-- (row, column) in traditional math notation order
	return (j-1) + 4 * (i-1)	-- store sequential in columns
end)
print(lambdaTest)
assert(not lambdaTest.rowmajor)
for i=0,15 do asserteq(lambdaTest.ptr[i], i) end

do local j=1 -- for j=1,3 do	-- A_{j,k}
	do local k=2 -- for k=j+1,4 do
		for swap=1,2 do
			print('j,k', j,k)
			local A_jk = math.random(20)
			local A_11 = math.random(3,7)

			local A = eye()
			A[{1,1}] = A_11
			A[{j,k}] = A_jk
			print()
			print'A'
			print('rowmajor', A.rowmajor)
			print(A)
			assert(not A.rowmajor)	-- colmajor by default still, right?
			asserteq(A.ptr[(j-1) + 4 * (k-1)], A_jk) 	-- colmajor means A_{1,2} goes at 0-based index 4

			local B_jk = math.random(20)
			local B_11 = math.random(3,7)
			local B = eye()
			B[{1,1}] = B_11
			B[{j,k}] = B_jk
			print()
			print'B'
			print('rowmajor', B.rowmajor)
			print(B)
			assert(not B.rowmajor)	-- colmajor by default still, right?

			local C = eye()
			C[{1,1}] = A_11 * B_11
			if swap==1 then
				C[{j,k}] = A_11 * B_jk + A_jk
			else
				C[{j,k}] = B_11 * A_jk + B_jk
			end

			local __mulresult = A * B
			print'__mulresult'
			print('rowmajor', __mulresult.rowmajor)
			print(__mulresult)
			asserteq(__mulresult, C)

			local mul4x4result = zero():mul4x4(A, B)
			print'mul4x4result'
			print('rowmajor', mul4x4result.rowmajor)
			print(mul4x4result)
			asserteq(mul4x4result, C)
			
			j,k = k,j
		end
	end
end

-- key = set/apply name, value = returns args used in either function
local r = math.random
for _,nf in ipairs{
	{Ortho = function() return r(-1000,1000),r(-1000,1000), r(-1000,1000), r(-1000,1000), r(-1000,1000) end},
	{Frustum = function() return r(-1000,1000),r(-1000,1000), r(-1000,1000), r(1,10), r(11,1000) end},
	{LookAt = function() return range(9):mapi(function(i) return r(-100,100) end):unpack() end},
	{Rotate = function() return r(-4,4), r(-4,4), r(-4,4), r(-4,4) end},
	{Scale = function() return r(-10,10), r(-10,10), r(-4, 4) end},
	{Translate = function() return r(-10,10), r(-10,10), r(-4, 4) end},
	{PickMatrix = function() return r(-10,10), r(-10,10), r(1, 10), r(1, 10) end},
	-- gets epsilon errors ... 
	{Perspective = function(fovy, aspectRatio, zNear, zFar)
		return r(45,90), r(1,10), r(1,10), r(11, 1000)
	end},
} do
	print()
	local n,f = next(nf)
	print(n)
	local fs = table.pack(f())
	local A = matrix{4,4}:lambda(function(i,j)
		return math.random(-10,10)
	end)
	print'A'
	print(A)
	local B = zero()
	assertindex(matrix,'set'..n)(B, fs:unpack())
	print'B'
	print(B)
	local C1 = A * B				-- test __mul, which always allocs a new matrix
	print'C1'
	print(C1)
	local C2 = zero():mul4x4(A, B)	-- test :mul4x4, which doesn't allocate but does always multiply, and test it with the :set function which gives us a 4x4 matrix
	print'C2'
	print(C2)
	local C3 = A:clone()
	assertindex(matrix, 'apply'..n)(C3, fs:unpack())	-- test ':apply' which optimizes the :set function
	print'C3'
	print(C3)
	asserteq(C1, C2, 'C1 vs C2')
	asserteq(C1, C3, 'C1 vs C3')
	asserteq(C2, C3, 'C2 vs C3')	-- meh
end
