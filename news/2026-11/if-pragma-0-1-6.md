# The bundled `if` pragma is re-vendored at 0.1.6

`modules/if/` now carries upstream `if:ver<0.1.6>:auth<zef:raku-community-modules>`
(tag `0.1.6`, commit `d97707f3e539bf5272c8f845ede19d7fefb3e95c`), copied
verbatim from the fez release, up from 0.1.5.

The release rewrites the body of the pragma's `statement-control:sym<use>`
actions method: the `:if` adverb no longer has to be the last colonpair of the
`use` statement, and its value is computed by wrapping the expression in a
`RakuAST::Block` and running it through `to-begin-time` / `meta-object` instead
of the internal `RakuAST::BeginTime.IMPL-BEGIN-TIME-EVALUATE`. mutsu reads the
actions role's method *names* and never runs the body (ADR-0026 §4, ADR-0098),
so the observable behaviour is unchanged. `batteries.lock` pins the new commit
and the upstream suite stays whole in the release gate.
