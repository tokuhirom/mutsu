# RakuAST bitwise negation prefixes round-trip

RakuAST lowering now distinguishes the prefix `+^`, `?^`, and `~^` operators
from the infix operators with the same spellings. Parsed and constructed prefix
nodes compile through the existing unary bytecode path.
