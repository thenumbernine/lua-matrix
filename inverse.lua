-- taken from my symmath matrix inverse
return function(A, A_det, b)
	local matrix = require 'matrix'
	local dim = A:size()
	if #dim ~= 2 or dim[1] ~= dim[2] then
		error("inv() only works for square matrices")
	end

	local m, n = dim[1], dim[2]

	A = matrix(A)

	-- assumes A is a rank-2 array with matching height
	local AInv = b and matrix(b) or matrix.eye{m}
	local invdim = AInv:size()
	assert(#invdim == 2, "expect b to be a rank-2 Array")
	if invdim[1] ~= dim[1] then
		if b then
			error("expected A number of rows to match b number of rows.\n"
				.."found A to have "..m.." rows and b to have "..invdim[1].." rows")
		else
			error("hmm, you picked the wrong default number of rows for the result")
		end
	end

	-- shortcuts:
	if not b then
		if m == 1 and n == 1 then
			local A_11 = A[1][1]
			if A_11 == 0 then
				return AInv, A, "determinant is zero"
			end
			local result = matrix{1/A_11}
			if b then result = result * b end
			return result, invdim:eye()
		elseif m == 2 and n == 2 then
			A_det = A_det or A:determinant()
			if A_det ~= 0 then
				local result = matrix(
					{A[2][2], -A[1][2]},
					{-A[2][1], A[1][1]}
				) / A_det
				--if b then result = result * b end
				return result, invdim:eye()
			end
-- TODO maybe put GaussJordan in one method and have inverse() and pseudoInverse() etc call it
-- then give inverse() its own shortcut for the 2x2 and 3x3 methods?
-- because right now this function does a few things: inverse, pseudoinverse, linear system solution
		elseif m == 3 and n == 3 then
			A_det = A_det or A:determinant()
			if A_det ~= 0 then
				-- transpose, +-+- sign stagger, for each element remove that row and column and
				local result = matrix(
					{A[2][2]*A[3][3]-A[2][3]*A[3][2], A[1][3]*A[3][2]-A[1][2]*A[3][3], A[1][2]*A[2][3]-A[1][3]*A[2][2]},
					{A[2][3]*A[3][1]-A[2][1]*A[3][3], A[1][1]*A[3][3]-A[1][3]*A[3][1], A[1][3]*A[2][1]-A[1][1]*A[2][3]},
					{A[2][1]*A[3][2]-A[2][2]*A[3][1], A[1][2]*A[3][1]-A[1][1]*A[3][2], A[1][1]*A[2][2]-A[1][2]*A[2][1]}
				) / A_det
				--if b then result = result * b end
				return result, invdim:eye()
			end
		end
	end

	local min = math.min(m,n)

	-- i is the column, which increases across the matrix
	-- row is the row, which only increases as we find a new linearly independent column
	local row = 1
	for i=1,min do
		-- if we have a zero on the diagonal...
		local found = true
		if A[row][i] == 0 then
			-- pivot with a row beneath this one
			found = false
			for j=row+1,m do
				if A[j][i] ~= 0 then
					for k=1,n do
						A[j][k], A[row][k] = A[row][k], A[j][k]
					end
					for k=1,invdim[2] do
						AInv[j][k], AInv[row][k] = AInv[row][k], AInv[j][k]
					end
					found = true
					break
				end
			end
		end
		if not found then
			-- return the progress if things fail
			--return AInv, A, "couldn't find a row to pivot"
		else
			-- rescale diagonal
			if A[row][i] ~= 1 then
				-- rescale column
				local s = A[row][i]
-- TODO verbose arg?
				for j=1,n do
					A[row][j] = A[row][j] / s
				end
				for j=1,invdim[2] do
					AInv[row][j] = AInv[row][j] / s
				end
			end
			-- eliminate columns apart from diagonal
			for j=1,m do
				if j ~= row then
					if A[j][i] ~= 0 then
						local s = A[j][i]
						for k=1,n do
							A[j][k] = A[j][k] - s * A[row][k]
						end
						for k=1,invdim[2] do
							AInv[j][k] = AInv[j][k] - s * AInv[row][k]
						end
					end
				end
			end
			row = row + 1
		end
	end

	if m > n then
		-- if there are more rows than columns
		-- then the remaining columns must be zero
		-- and should be cut out?
		for i=n+1,m do
			for j=1,invdim[2] do
				if AInv[i][j] ~= 0 then
					return AInv, A, "system is overconstrained"
				end
			end
		end
	end

	return AInv, A
end
