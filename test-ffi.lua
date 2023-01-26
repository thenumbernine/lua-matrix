#!/usr/bin/env luajit

-- TODO put this in matrix/matrix_ffi ?

local matrix_ffi = require 'matrix.ffi'
require 'complex'	-- setup metatype and operators for ffi complex types

--[[ TODO?
-- in a perfect would this is just a matrix-exponent ....
-- ... but a matrix-exponent is implemented as an eigen-decomposition and scalar-exponent of eigenvalues
-- ... and for rotations, those eigenvalues are complex
-- ... and for some weird reason, for complex eigenvalues eigensystems, lapack's implementation is crapping itself
local A = matrix_ffi.rotate(math.pi/2, 0,0,1)
--]]
-- [[ until then
local A = matrix_ffi({
	{0,-1,0},
	{1,0,0},
	{0,0,1}
}, 'complex double')
--]]

print('A\n'..A)
print()

local Ainv = A:inv()
print('Ainv\n'..Ainv)
print('A * Ainv\n'..(A * Ainv))
print('A * Ainv\n'..matrix_ffi(A * Ainv, 'double'))
print()

print('A\n'..A)
local U, S, V = A:svd()
print('U\n'..U)
print('S\n'..S)
print('V\n'..V)
local USVcplxd = U*S:diag()*V
print('U * S * V\n'..USVcplxd)
print('U * S * V\n'..matrix_ffi(USVcplxd, 'double'))
print()

-- hmm this doesnt decompose my cplx-eigenvalue matrix
print('A\n'..A)
local alpha, VR, VL, beta = A:eig()
print('alpha\n'..alpha)
print('VL\n'..VL)
print('VR\n'..VR)
print('beta\n'..beta)
print('alpha\n'..alpha:diag())
local RAL = VR * alpha:diag() * VL
-- so our left-eigenvectors aren't necessarily the inverse of the right ... ?
-- and that means A = R Lambda L isnt true?
-- but A = R Lambda R^-1 is?
print('VR * alpha * VL\n'..RAL)
print('VR * alpha * VL\n'..matrix_ffi(RAL, 'double'))
print('VR * alpha * VR:inv()\n'..(VR * alpha:diag() * VR:inv()))
print('VR * alpha * VR:inv()\n'..matrix_ffi(VR * alpha:diag() * VR:inv(), 'double'))

--[[
print('A\n'..A)
local L, U = A:lu()
print('L\n'..L)
print('U\n'..U)
local LU = L * U
print('L * U\n'..LU)
print('L * U\n'..matrix_ffi(LU, 'double'))
--]]

-- what about diagonalization
-- and what about boosts
-- and combining them?
