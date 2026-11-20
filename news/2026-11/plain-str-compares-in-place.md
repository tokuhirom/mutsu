# Two plain strings compare in place

`$ch eq ' '` with two plain `Str` operands used to go through the whole
string-comparison path, with these steps:

- pop both operands;
- ask for a user candidate;
- check both for a Junction and for a Blob;
- borrow both through `str_context_cow`;
- compare them.

A tokenizer predicate made of five such compares spent about 360
instructions on each one.

The comparison ops now answer two plain `Str`s on the stack directly, as long
as no user candidate of the operator is in scope (ADR-18357 stage 2). They
call the same `str_prim::str_eq` / `str_order` routines the general path
ends in. These operands still take the general path, which coerces or
threads them as before:

- an allomorph, a mixin, a Junction or a Blob;
- any operand when a declared or imported `infix:<eq>` / `infix:<lt>` / ...
  is in scope.

`MUTSU_NARROW_OPS=0` turns this off along with the `my int` steps, and
`t/vm/codegen/opcode-narrow-form-parity.t` runs every pair of a mixed word
list through all six operators both ways.

Callgrind slopes on one profiling build, narrow forms on against off:

| loop | on | off |
| --- | ---: | ---: |
| `is-space($c)` | 9,321 | 9,878 |
| `is-name-char($c)` | 10,587 | 11,121 |

That is about 110 instructions saved per compare. Most of what an `eq` still
costs is copying its operands onto the stack and dropping them again. A
fused local-against-literal compare is the next step.
