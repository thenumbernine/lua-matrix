local class = require 'ext.class'
local table = require 'ext.table'
local range = require 'ext.range'
-- wild card singleton
local _ = range

local _m = class()
function _m:init(t, ...)
	if type(t) == 'table' then
		for i=1,#t do
			if type(t[i]) == 'table' then
				self[i] = _m(t[i])
			else
				self[i] = t[i]
			end
		end
	end
end
function _m:__tostring()
	return '[' .. table(self):map(tostring):concat',' .. ']'
end
function _m.__concat(a,b)
	return tostring(a) .. tostring(b)
end
function _m:__call(i, ...)
	if i == _ then
		local copy = _m()
		for i=1,#self do
			copy[i] = self(i,...)
		end
		return copy
	elseif type(i) == 'table' then
		local copy = _m()
		for j,k in ipairs(i) do
			copy[j] = self(k,...)
		end
		return copy
	end
	
	if select('#', ...) == 0 then
		return self[i]
	end
	return self[i](...)
end

t = _m{1,2,3}
print(t)

t = _m{{1,2,3},{4,5,6},{7,8,9}}
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

t = _m{{{1,2},{3,4}},{{5,6},{7,8}}}
print(t(1,_,_))
print(t(2,_,_))
print(t(_,1,_))
print(t(_,2,_))
print(t(_,_,1))
print(t(_,_,2))
print(t(_,_,_))
