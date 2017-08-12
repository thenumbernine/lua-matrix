local matrix = require 'matrix'
return function(A,dx)
	local size = A:size()
	return size:lambda(function(...)
		local i = matrix{...}
		return matrix{#size}:lambda(function(j)
			if i[j] == 1 or i[j] == size[j] then return 0 end
			local ip = matrix(i) ip[j] = ip[j] + 1
			local im = matrix(i) im[j] = im[j] - 1
			local result = .5 * dx[j] * (A[ip] - A[im])
			return result
		end)
	end)
end
