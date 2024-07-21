package = "matrix"
version = "dev-1"
source = {
	url = "git+https://github.com/thenumbernine/lua-matrix"
}
description = {
	summary = "Matrix class for Lua",
	detailed = "Matrix class for Lua",
	homepage = "https://github.com/thenumbernine/lua-matrix",
	license = "MIT"
}
dependencies = {
	"lua >= 5.1",
}
build = {
	type = "builtin",
	modules = {
		["matrix.curl"] = "curl.lua",
		["matrix.determinant"] = "determinant.lua",
		["matrix.div"] = "div.lua",
		["matrix.ffi"] = "ffi.lua",
		["matrix.grad"] = "grad.lua",
		["matrix.helmholtzinv"] = "helmholtzinv.lua",
		["matrix.index"] = "index.lua",
		["matrix.inverse"] = "inverse.lua",
		["matrix.lapinv"] = "lapinv.lua",
		matrix = "matrix.lua",
		["matrix.tests.compare-speed"] = "tests/compare-speed.lua",
		["matrix.tests.test"] = "tests/test.lua",
		["matrix.tests.test-ffi"] = "tests/test-ffi.lua"
	},
}
