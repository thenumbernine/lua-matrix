local matrix = require 'matrix'
return function(A,dx)
	local size = A:size()
	local n = table.remove(size)
	return size:lambda(function(...)
		local i = matrix{...}
		for j=1,n do
			if i[j] == 1 or i[j] == size[j] then return 0 end
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
