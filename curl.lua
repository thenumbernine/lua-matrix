return function(A,dx)
	local matrix = getmetatable(A)
	assert(A:degree() == 4)
	local size = A:size()
	local sx,sy,sz,sv = size:unpack()
	assert(sv == 3)
	assert(dx:degree() == 1)
	assert(dx:len() >= 3)
	return size:lambda(function(x,y,z,i)
		if x==1 or x==sx
		or y==1 or y==sy
		or z==1 or z==sz
		then 
			return 0
		end
		local j = i%3+1
		local k = j%3+1
		local jp = matrix{x,y,z} jp[j]=jp[j]+1
		local jm = matrix{x,y,z} jm[j]=jm[j]-1
		local kp = matrix{x,y,z} kp[k]=kp[k]+1
		local km = matrix{x,y,z} km[k]=km[k]-1
		return .5 * (dx[j] * (A(jp[1],jp[2],jp[3],k) - A(jm[1],jm[2],jm[3],k))
					- dx[k] * (A(kp[1],kp[2],kp[3],j) - A(km[1],km[2],km[3],j)))
	end)
end
