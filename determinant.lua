-- originally from my symmath project
local function determinant(m)
	local matrix = require 'matrix'
	
	local dim = m:size()
	if #dim == 0 then return 1 end
	
	local volume = #dim == 0 and 0 or dim:prod()
	if volume == 0 then return 1 end

	if #dim ~= 2 then 
		error("dim is "..#dim..' with value '..m[1][1]) 
	end

	-- not square?
	if dim[1] ~= dim[2] then
		error("determinant only works on square matrices")
	end

	local n = dim[1]

	-- 1x1 matrix? 
	if n == 1 then
		return m[1][1]
	elseif n == 2 then
		return m[1][1] * m[2][2] - m[1][2] * m[2][1]
	end

	-- pick row (or column) with most zeroes
	local mostZerosOfRow = -1
	local mostZerosOfCol = -1
	local rowWithMostZeros
	local colWithMostZeros
	for i=1,n do
		local sumRow = 0
		local sumCol = 0
		for j=1,n do
			if m[i][j] == 0 then sumRow = sumRow + 1 end
			if m[j][i] == 0 then sumCol = sumCol + 1 end
		end
		if sumRow > mostZerosOfRow then
			mostZerosOfRow = sumRow
			rowWithMostZeros = i
		end
		if sumCol > mostZerosOfCol then
			mostZerosOfCol = sumCol
			colWithMostZeros = i
		end
	end
	
	local useCol = mostZerosOfCol > mostZerosOfRow
	local x = useCol and colWithMostZeros or rowWithMostZeros

	local results = 0
	for y=1,n do
		local i,j
		if useCol then i,j = y,x else i,j = x,y end
		local mij = m[i][j]
		-- if the # of flips is odd then scale by -1, if even then by +1 
		local sign = ((i+j)%2)==0 and 1 or -1
		if mij ~= 0 then
			local submat = matrix()
			for u=1,n-1 do
				local submat_u = matrix()
				submat[u] = submat_u
				for v=1,n-1 do
					local p,q = u,v
					if p>=i then p=p+1 end
					if q>=j then q=q+1 end
					submat_u[v] = m[p][q]
				end
			end
			local subres = determinant(submat)
			results = results + sign * mij * subres
		end
	end
	return results
end

return determinant
