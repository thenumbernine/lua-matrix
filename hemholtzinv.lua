-- for an n-dimensional scalar matrix
-- adds an extra dimension of the gradient of the function
-- extra dimension size is scalar matrix's degree
local curl = require 'matrix.curl'
local grad = require 'matrix.grad'
local lapinv = require 'matrix.lapinv'
local matrix = require 'matrix'
local table = require 'ext.table'

--[[
-- functions for finding the inverse Hemholtz decomposition 
-- finds the inverse of a curl
-- given a grid of [i1...in, j] for dim(j) = 3
-- returns the like-dimensioned inverse of the curl

... so how do you perform a discrete inverse of div and curl?
https://groups.google.com/forum/#!topic/comp.soft-sys.matlab/jv_2gsSF-pE
curl B = R
div B = D
(A,phi) = Hemholtz decomposition of B
B = curl A + grad phi
first the curl ...
div A = 0 <= gauge
curl B = R <=> curl(curl A + grad phi) = R <=> curl curl A = R <=> grad div A - lap A = R <=> -lap A = R
=> A = -veclap^-1 R ... lap is applied per-component
next the div ...
div B = D <=> div (curl A + grad phi) = D <=> div grad phi = lap phi = D
=> phi = lap^-1 D
B = curl A + grad phi = grad lap^-1 D - curl veclap^-1 R
--]]
return function(args)
	local D = assert(args.div)
	local R = assert(args.curl)
	local dx = assert(args.dx)

	local size
	if D then
		size = D:size()
	end
	if R then
		local size2 = R:size()
		assert(size2[#size2] == 3)
		size2[#size2] = nil
		if size then
			assert(size == size2)
		else
			size = size2
		end
	end

	-- needs to be 3D for div or curl to work
	assert(#size == 3)

	local result
	if R then
		-- rearrange R's so the vector indexes are first
		R = matrix{3,size[1],size[2],size[3]}:lambda(function(...)
			local i = table{...}
			i:insert(i:remove(1))
			return R[i]
		end)
		
		-- A = -veclap^-1 R 
		local A = matrix{3}:lambda(function(i)
			return -lapinv(table(args, {lap=R[i], dx=dx}))
		end)

		-- reshape back
		A = matrix{size[1],size[2],size[3],3}:lambda(function(...)
			local i = table{...}
			i:insert(1, i:remove())
			print('A['..i..'] = '..A[i])
			return A[i]
		end)

		local curlA = curl(A,dx)
		result = curlA
	end	
	if phi then
		local phi = D and lapinv(table(args, {lap=D, dx=dx})) or nil
		local gradPhi = grad(phi, dx)
		result = result and result + gradPhi or gradPhi
	end	
	return result 
end
