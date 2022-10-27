### Lua Matrix libray, in the spirit of Matlab

[![Donate via Stripe](https://img.shields.io/badge/Donate-Stripe-green.svg)](https://buy.stripe.com/00gbJZ0OdcNs9zi288)<br>
[![Donate via Bitcoin](https://img.shields.io/badge/Donate-Bitcoin-green.svg)](bitcoin:37fsp7qQKU8XoHZGRQvVzQVP8FrEJ73cSJ)<br>

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
