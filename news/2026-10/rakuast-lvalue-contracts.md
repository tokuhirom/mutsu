# RakuAST keeps lvalue and subscript adverb contracts

Conditional assignment expressions retain their writable container when a
statement modifier is nested inside a compound-assignment target. A taken
branch receives the update; an untaken branch remains a no-op.

Bare `Any` type receivers and lexical type aliases resolve through the term
namespace across RakuAST lowering and EVAL. This keeps immutable type objects
from borrowing a same-named scalar's container while preserving lexical
shadowing and sigilless bindings.

Multi-dimensional subscripts with the invalid `:!exists:delete` combination
retain their index and source-ordered colonpairs across conversion. Source and
constructed RakuAST nodes raise the ordinary `X::Adverb` exception. The three
existing failing frontend files and the new oracle-backed contract test now
pass in round-trip mode.
