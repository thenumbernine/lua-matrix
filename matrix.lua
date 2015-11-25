local class = require 'ext.class'
local table = require 'ext.table'
local _ = require 'matrix.index'

local matrix = class()

function matrix:init(t, ...)
	if type(t) == 'table' then
		for i=1,#t do
			if type(t[i]) == 'table' then
				self[i] = matrix(t[i])
			else
				self[i] = t[i]
			end
		end
	end
end

function matrix:__tostring()
	return '[' .. table(self):map(tostring):concat',' .. ']'
end

function matrix.__concat(a,b)
	return tostring(a) .. tostring(b)
end

function matrix:__call(i, ...)
	if i == _ then i = _(#self) end
	if type(i) == 'table' then
		local copy = matrix()
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

function matrix:__index(i)
	if type(i) ~= 'table' then return rawget(self,i) or rawget(matrix,i) end
	return self(table.unpack(i))
end

function matrix:__newindex(i,v)
	if i == _ then i = {_(#self)} end	-- range ref should only be used from outside this method
	if type(i) == 'table' then
		local ii = i[1]
		if ii == _ then ii = _(#self) end
		if type(ii) == 'number' then
			if #i > 1 then
				self[ii][{table.unpack(i,2)}] = v
			else
				self[ii] = v
			end
		else
			for j,k in ipairs(ii) do
				if #i > 1 then
					self[k][{table.unpack(i,2)}] = type(v) == 'number' and v or v[k]
				else
					self[k] = type(v) == 'number' and v or v[k]
				end
			end
		end
	else
		rawset(self,i,v)
	end
end

return matrix
