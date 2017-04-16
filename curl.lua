local matrix = require 'matrix'
return function(A,dx)
	local dx,dy,dz = dx:unpack()
	local size = A:size()
	assert(#size == 4)
	assert(table.remove(size) == 3)
	return size:lambda(function(i,j,k)
		if i==1 or i==size[1]
		or j==1 or j==size[2]
		or k==1 or k==size[3]
		then 
			return matrix{0,0,0}
		end
		return matrix{
			.5 * (dy * (A[i][j+1][k][3] - A[i][j-1][k][3])
				- dz * (A[i][j][k+1][2] - A[i][j][k-1][2])),
			.5 * (dz * (A[i][j][k+1][1] - A[i][j][k-1][1])
				- dx * (A[i+1][j][k][3] - A[i-1][j][k][3])),
			.5 * (dx * (A[i+1][j][k][2] - A[i-1][j][k][2])
				- dy * (A[i][j+1][k][1] - A[i][j-1][k][1])),
		}
	end)
end
