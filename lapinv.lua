local gmres = require 'solver.gmres'
return function(args)
	local lap = assert(args.lap)
	local dx = assert(args.dx)
	local matrix = getmetatable(lap)
	local size = lap:size()
	local n = size:len()
	args.x = args.x or -lap
	args.b = args.b or lap
	args.A = args.A or function(y)
		return size:lambda(function(...)
			local i = matrix{...}
			for j=1,n do
				if i[j] == 1 or i[j] == size[j] then return 0 end
			end
			local sum = 0
			for j=1,n do
				local ip = matrix(i) ip[j] = ip[j] + 1
				local im = matrix(i) im[j] = im[j] - 1
				sum = sum + dx[j] * dx[j] * (y[ip] - 2*y[i] + y[im])
			end
			return sum
		end)
	end
	args.clone = args.clone or matrix
	args.dot = args.dot or matrix.dot
	args.epsilon = args.epsilon or 1e-30
	args.maxiter = args.maxiter or 4 * size:prod()
	args.restart = args.restart or 50
	local solver = args.solver or gmres
	return solver(args)
end
