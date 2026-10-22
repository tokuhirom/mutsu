RakuAST lowering now preserves a sigilless pointy block's parameter metadata so
the closure binder can write through array and hash element arguments. The
regression passes for stored closures, code-variable and ampersand calls, and
immediate calls.
