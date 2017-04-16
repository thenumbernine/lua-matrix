local matrix = require 'matrix'
return function(A,dx)
	local size = A:size()
	assert(#size == 4)
	assert(table.remove(size) == 3)
	return size:lambda(function(...)
		local i = matrix{...}
		local Ai = A(i)
		for j=1,3 do
			if i[j] == 1 or i[j] == size[j] then
				return matrix{0,0,0}
			end
		end
		return matrix{3}:lambda(function(j)
			local j2 = j%3+1
			local j3 = j2%3+1
			local ip = matrix(i) ip[j2]=ip[j2]+1
			local im = matrix(i) im[j2]=im[j2]+1
			return .5 * dx[j2] * (A[ip][j3] - A[im][j3])
		end)
	end)
end
