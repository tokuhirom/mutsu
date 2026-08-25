# `check-value-wall.sh` no longer counts prose as a wall violation

`scripts/check-value-wall.sh` ratchets the number of direct `Value::<Variant>`
uses outside `src/value/` down toward zero. Its regex matched the variant token
anywhere in a file, including inside comments, so a doc comment that merely
*named* a variant tripped the ratchet. On one clean `main` checkout that made
`make test` fail before running a single test, on two `//`/`//!` prose lines in
`src/vm/vm_hash_subclass_delegate.rs`. CI never invokes the script, so the
breakage was invisible there and only hit anyone following the documented local
workflow.

Both offending comments have since been reworded away, so the symptom no longer
reproduces — but the script was still one prose reference away from breaking
`make test` again. It now strips whole-line `//` comments before counting.

The strip is deliberately conservative: only lines whose first non-whitespace is
`//` are dropped, never a trailing `// ...` on a code line. Cutting at the first
`//` anywhere on a line would have silently weakened the ratchet, because a line
such as `let u = "https://x"; Value::Int(1)` would lose its real violation along
with the false `//`. That case is exercised directly — appending a comment-only
prose line leaves the count at `0`, while appending the URL-then-`Value::Int`
code line correctly fails the ratchet with exit 1. Occurrence counting (rather
than line counting) is preserved, so two violations on one line still count as
two.
