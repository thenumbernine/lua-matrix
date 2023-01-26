local matrix = require 'matrix'
local class = require 'ext.class'

local Dirichlet = class()

function Dirichlet:init(value)
	self.value = value or 0
end

function Dirichlet:__call(A, i, size, n)
	return self.value
end


local Newmann = class()

function Newmann:__call(A, i, size, n)
	for j=1,n do

	end
end

local bcClasses= {
	Dirichlet = Dirichlet,
	Newmann = Newmann,
}

--[[
A = matrix of size m_1 x ... x m_n x n: to calcluate divergence, of size
dx = matrix of size n: step size along each of the grid axii
boundaryCondition = what to do when the boundary is reached.  default 'Dirichlet'
	accepts a string as the boundary condition name, or a table with .name equal to the boundary condition name and other args passed to the boundary condition object.
--]]
return function(A,dx,bcArg)
	local bcParams = type(bcArg) == 'table' and bcArg or nil
	local bcName = type(bcArg) == 'table' and bcArg.name or (type(bcArg) == 'string' and bcArg or 'Dirichlet')
	local bcClass = assert(bcClasses[bcName], "failed to find boundary condition class named "..bcName)
	local bc = bcClass(bcParams)
	local size = A:size()
	local n = table.remove(size)
	return size:lambda(function(...)
		local i = matrix{...}
		for j=1,n do
			if i[j] == 1 or i[j] == size[j] then
				return bc(A,i,size,n)
			end
		end
		local sum = 0
		for j=1,n do
			local ip = matrix(i) ip[j] = ip[j] + 1
			local im = matrix(i) im[j] = im[j] - 1
			sum = sum + .5 * dx[j] * (A[ip][j] - A[im][j])
		end
		return sum
	end)
end
