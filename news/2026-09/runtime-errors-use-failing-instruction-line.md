# Runtime errors use the failing instruction's source line

Backtraces now locate a runtime error at the bytecode instruction that raised it. A typed assignment on a later line, including an array element store, no longer reports the line of an earlier statement when the store opcode returns an error.
