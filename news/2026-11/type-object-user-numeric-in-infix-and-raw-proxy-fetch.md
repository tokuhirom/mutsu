# Infix numeric operators call a type object's user `Numeric`; `+` fetches a raw Proxy result

`class C { method Numeric { 3 } }; C + 2` gave 2: only prefix `+` called the user `Numeric` of a bare
type object, the infix operators numified it to 0. They now call it, as rakudo does.
`+get()` over `sub get() is raw { $proxy }` also returned 0: the Proxy came back wrapped in the
call's container and was not FETCHed. Both are what Universal::Errno's `+errno` needs.
