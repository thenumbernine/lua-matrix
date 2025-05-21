#!/usr/bin/env luajit
--[[
just for terms sake:
row-major = memory iterates rows first, then columns.  This is "C" standard for what you read when you list-initializ an array.
col-major = memory iterates columns first, then rows.  This is OpenGL's standard.

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
local assert = require 'ext.assert'
local matrix = require 'matrix.ffi'
local matrix_lua = require 'matrix'
local function zero()
	return matrix{4,4}:zeros()
end
local function eye()
	return matrix{4,4}:eye()
end

-- testing default column-major storage
-- matrix:lambda args are (row, column) in traditional math notation order
local lambdaTest = matrix{4,4}:lambda(function(i, j)
	return (i-1) + 4 * (j-1)	-- store sequential in columns
end)
print(lambdaTest)
print('memory:', range(0,15):mapi(function(i) return lambdaTest.ptr[i] end):concat', ')
print(lambdaTest.rowmajor and 'row' or 'column', 'major')
assert(not lambdaTest.rowmajor)
for i=0,15 do
	assert.eq(lambdaTest.ptr[i], i)
end

-- make sure it works with vector-mul:
-- ... rhs col vec mul with matrix-ffi
local result = lambdaTest * matrix{1,2,3,4}
print('result', result)
assert.eq(result, matrix{80, 90, 100, 110})
-- ... rhs col vec mul with matrix-lua
local result = lambdaTest * matrix_lua{1,2,3,4}
print('result', result)
assert.eq(result, matrix{80, 90, 100, 110})
-- ... lhs row vec mul with matrix-ffi
local result = matrix{1,2,3,4} * lambdaTest
print('result', result)
assert.eq(result, matrix{20, 60, 100, 140})
-- ... lhs row vec mul with matrix-lua
--[[ hmm I think this one is doing the matrix-lua __mul pathway
local result = matrix_lua{1,2,3,4} * lambdaTest
print('result', result)
assert.eq(result, matrix{20, 60, 100, 140})
--]]

-- testing row-major storage
local lambdaTest = matrix{4,4}:lambda(function(i, j)
	return (j-1) + 4 * (i-1)	-- store sequential in rows
end, nil, nil, true)	-- 5th 'true' means row-major
print(lambdaTest)
print('memory:', range(0,15):mapi(function(i) return lambdaTest.ptr[i] end):concat', ')
print(lambdaTest.rowmajor and 'row' or 'column', 'major')
assert(lambdaTest.rowmajor)
for i=0,15 do
	assert.eq(lambdaTest.ptr[i], i)
end

-- make sure it works with vector-mul:
-- ... rhs col vec mul with matrix-ffi
local result = lambdaTest * matrix{1,2,3,4}
print('result', result)
assert.eq(result, matrix{20, 60, 100, 140})
-- ... rhs col vec mul with matrix-lua
local result = lambdaTest * matrix_lua{1,2,3,4}
print('result', result)
assert.eq(result, matrix{20, 60, 100, 140})
-- ... lhs row vec mul with matrix-ffi
local result = matrix{1,2,3,4} * lambdaTest
print('result', result)
assert.eq(result, matrix{80, 90, 100, 110})
-- ... lhs row vec mul with matrix-lua
--[[ hmm I think this one is doing the matrix-lua __mul pathway
local result = matrix_lua{1,2,3,4} * lambdaTest
print('result', result)
assert.eq(result, matrix{80, 90, 100, 110})
--]]

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
			assert.eq(A.ptr[(j-1) + 4 * (k-1)], A_jk) 	-- colmajor means A_{1,2} goes at 0-based index 4

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
			assert.eq(__mulresult, C)

			local mul4x4result = zero():mul4x4(A, B)
			print'mul4x4result'
			print('rowmajor', mul4x4result.rowmajor)
			print(mul4x4result)
			assert.eq(mul4x4result, C)
			
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
	assert.index(matrix,'set'..n)(B, fs:unpack())
	print'B'
	print(B)
	local C1 = A * B				-- test __mul, which always allocs a new matrix
	print'C1'
	print(C1)
	local C2 = zero():mul4x4(A, B)	-- test :mul4x4, which doesn't allocate but does always multiply, and test it with the :set function which gives us a 4x4 matrix
	print'C2'
	print(C2)
	local C3 = A:clone()
	assert.index(matrix, 'apply'..n)(C3, fs:unpack())	-- test ':apply' which optimizes the :set function
	print'C3'
	print(C3)
	local function normcmp(a,b) return (a - b):norm() end
	local eps = 1e-5	-- 1e-7 was too tight
	assert.eqepsnorm(C1, C2, eps, normcmp, 'C1 vs C2')
	assert.eqepsnorm(C1, C3, eps, normcmp, 'C1 vs C3')
	assert.eqepsnorm(C2, C3, eps, normcmp, 'C2 vs C3')
end
