### Lua Matrix libray, in the spirit of Matlab

### Dependencies:

- https://github.com/thenumbernine/lua-ext
- matrix.ffi depends on LuaJIT, otherwise this works with vanilla Lua
- matrix.lapinv depends on https://github.com/thenumbernine/solver-lua (which itself depends on this project)

### Example:

matrix initialization:

`m = matrix{1,2,3}`

`m = matrix{{1,2},{3,4}}`

`m = matrix(n)`

etc

`m = matrix.const(math.pi, 2, 3)`

`m = matrix.const(math.pi, {2,2})`

gives you a 2x2 matrix filled with the value for pi.

(TODO finish this)
