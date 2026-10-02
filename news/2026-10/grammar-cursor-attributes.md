# A grammar method's attribute writes land on the Match of the rule that called it

A grammar's cursor is an instance of the grammar, so what a method a rule calls
as a subrule writes to `$!attr` belongs to the cursor of that rule invocation,
and that cursor is the rule's Match (#9803, the last part of ADR-0135 Slice D):

```raku
grammar G {
    has $.inv;
    token TOP { <t> }
    token t { a <.acc> }
    method acc { $!inv = True; self }
}
say G.parse("a")<t>.inv;   # True   (mutsu used to print Nil)
say G.parse("a").inv;      # (Any)  TOP's own cursor was never written
```

Each invocation owns its cursor: two matches of one token each start from the
uninitialised attribute, calls from one invocation accumulate on it, and a
failed branch leaves nothing on the winner. The documented `HTTPRequest` example
(`Language/grammars.rakudoc`, "Attributes in grammars") now prints what raku
prints.

The compiled engine owns the instance in the `Frame` of the call, created the
first time a call in that frame runs a grammar method, so a rule that never calls
one pays nothing. The walk, which still evaluates the rules the compiled engine
bridges to it (a `<-crlf>` class, call arguments, a wrapped token, ...), opens the
same scope around each rule invocation it evaluates. The instance goes onto the
callee's capture node, and the Match materializes its attributes from it.

A cursor is created, not built, as in raku: every declared attribute is present
as its uninitialised value (the type object, an empty `@` / `%`), and a `has $.x
= 5` default is not applied. An attribute no method wrote used to read `Nil`
off a grammar Match; it now reads like raku (`Any`, `Int`, `[]`, `{}`).

Not covered, filed separately: a method inherited from a parent grammar is not
found as a subrule of a derived one (#10508), and `self` inside a token's `{ ... }`
code block is not the cursor (#10509).
