#!/usr/bin/env luajit
--[[
i made matrix.ffi to hopefully save on space and speed
but made the mistake of trying to make the matrix.ffi API compatible with the matrix.matrix
which means 1-based indexes and a bunch of other nasty stuff
--]]
local timer = require 'ext.timer'
local table = require 'ext.table'
local range = require 'ext.range'
local gnuplot = require 'gnuplot'
local matrix = require 'matrix'
local matrix_ffi = require 'matrix.ffi'

local ns = range(200)

local tests = table()
for _,info in ipairs{
	{name='matrix', matrixClass=matrix},
	{name='matrix_ffi', matrixClass=matrix_ffi},
} do
	tests:insert{
		name = info.name,
		matrixClass = info.matrixClass,
		build = table(),
		mul = table(),
	}
end

-- a better way to suppress output?
timer.out = {
	write = function() end,
	flush = function() end,
}

for _,n in ipairs(ns) do
	for _,test in ipairs(tests) do
		local matrixClass = test.matrixClass
		local a, b, c
		local buildTime = timer('building', function()
			a = matrixClass.lambda({n,n}, function(i,j) return i + j end)
			b = matrixClass.lambda({n,n}, function(i,j) return i + j end)
		end)
		local mulTime = timer('multiplying new', function()
			c = a * b
		end)
		test.build:insert(buildTime)
		test.mul:insert(mulTime)
	end
end

for _,k in ipairs{'build', 'mul'} do
	gnuplot{
		terminal = 'svg size 1024,768',
		output = 'compare-time-'..k..'.svg',
		style = 'data lines',
		title = k..' time',
		xlabel = 'square matrix width',
		ylabel = 'time',
		log = 'y',
		data = matrix{
			ns,
			tests[1][k],
			tests[2][k],
		},
		{using='1:2', title=tests[1].name},
		{using='1:3', title=tests[2].name},
	}
end
