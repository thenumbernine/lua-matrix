#!/usr/bin/env luajit
local matrix = require 'matrix'
local _ = require 'matrix.index'

--[[
t = matrix{1,2,3}
print(t)
--]]

t = matrix{{1,2,3},{4,5,6},{7,8,9}}
--[[
print(t)
print(t(_))
print(t(_,_))
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
--]]

print('test case')
t[{_(1,2),_(2,3)}] = 0
print(t)
os.exit()

t = matrix{{{1,2},{3,4}},{{5,6},{7,8}}}
print(t(1,_,_))
print(t(2,_,_))
print(t(_,1,_))
print(t(_,2,_))
print(t(_,_,1))
print(t(_,_,2))
print(t(_,_,_))
