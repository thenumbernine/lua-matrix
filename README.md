### Lua Matrix libray, in the spirit of Matlab

[![Donate via Stripe](https://img.shields.io/badge/Donate-Stripe-green.svg)](https://buy.stripe.com/00gbJZ0OdcNs9zi288)<br>

### Dependencies:

- depends on https://github.com/thenumbernine/lua-ext
- matrix.ffi depends on LuaJIT, otherwise this works with vanilla Lua
- matrix.lapinv depends on https://github.com/thenumbernine/solver-lua (which itself depends on this project)
- matrix.ffi has optional LAPACKE support which depends on https://github.com/thenumbernine/lua-ffi-bindings and on LAPACKE as well
- if you use a complex type with matrix.ffi then this library is required: https://github.com/thenumbernine/complex-lua

### Example:

matrix initialization:

`m = matrix{1,2,3}`

`m = matrix{{1,2},{3,4}}`

`m = matrix(n)`

etc

`m = matrix.const(math.pi, 2, 3)`

`m = matrix.const(math.pi, {2,2})`

gives you a 2x2 matrix filled with the value for pi.

(TODO more)
