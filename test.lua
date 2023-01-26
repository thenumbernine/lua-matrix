#!/usr/bin/env luajit
local matrix = require 'matrix'
local _ = require 'matrix.index'

local t
t = matrix{1,2,3}
assert(t == matrix{1,2,3})

t = matrix{{1,2,3},{4,5,6},{7,8,9}}
assert(t == matrix{{1,2,3},{4,5,6},{7,8,9}})
assert(t(_) == matrix{{1,2,3},{4,5,6},{7,8,9}})
assert(t(_,_) == matrix{{1,2,3},{4,5,6},{7,8,9}})
for i=1,3 do
	print(t(i))
	print(t(i,_))
	print(t(_,i))
	for j=1,3 do
		print(t(i,j))
	end
end
print(t(_(1,2),_))
print(t(_(2,3),_))
print(t(_,_(1,2)))
print(t(_,_(2,3)))

print('test case')
t[{_(1,2),_(1,3,2)}] = 0
print(t)

t = matrix{{{1,2},{3,4}},{{5,6},{7,8}}}
print(t(1,_,_))
print(t(2,_,_))
print(t(_,1,_))
print(t(_,2,_))
print(t(_,_,1))
print(t(_,_,2))
print(t(_,_,_))
