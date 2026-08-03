# A `for -> $k, $v` loop no longer inherits an unrelated lexical's type

A multi-parameter `for` loop rejected values whose type had nothing to do with
the loop:

```raku
sub declares-typed-v() { my Int $v = 42; $v }
say declares-typed-v();

for ("content-length", 10, "transfer-encoding", "identity") -> $k, $v {
    say "$k=$v";
}
# content-length=10
# Type check failed in assignment to $v; expected Int but got Str ("identity")
```

Any `my Int $v` / `state Int $v` / `our Int $v` *anywhere in the program* —
including in an unrelated module the script merely loaded — constrained every
subsequent `-> $k, $v` loop parameter.

## Root cause

The single-parameter form (`for @a -> $x`) binds natively in the VM. The
multi-parameter form does not: `build_for_bind_stmts`
(`src/compiler/mod.rs`) emits a plain `Stmt::Assign` per parameter into the body
prefix, and `SetLocal` type-checks an assignment against
`var_type_constraint(name)` — a **name-keyed, not block-scoped** map. A
parameter is a fresh binding that shadows whatever the name meant outside, so
inheriting that entry was wrong.

## Fix

`exec_for_loop_body` now saves each `multi_param_names` entry's constraint,
clears it for the duration of the loop, and restores it afterwards — the same
contract `bind_param_type_constraint` gives an untyped routine parameter, minus
the permanent loss of the enclosing lexical's type (an outer `my Int $v` is still
enforced once the loop has exited).

Pin: `t/for-multi-param-type-constraint.t` (6 tests, green under `raku` too).

## How it was found

The vendored Cro::HTTP suite's `t/http-rawbodyparserselector.rakutest` could not
run a single test: it died in

```raku
for %headers.kv -> $k, $v { $resp.append-header($k, $v) }
```

with `expected Int but got Str ("chunked")`. The `Int` came from
`OpenSSL::Stack`'s `state Int $v = OpenSSL::Version::version_num()`, three
modules away. That file now passes 10/10.

## Left open

Two related findings from the same investigation are filed rather than fixed
here, because each is its own piece of work:

- `todo/tickets/for-multi-param-shadow-clobbers-outer-lexical.md` — the loop
  parameter also clobbers the enclosing lexical's *value* (`my $v = "outer"; for
  (1,2) -> $k, $v { }; say $v` prints the uninitialized element). The env-side
  save/restore is already there; the local-slot half needs the shadowing slot
  resolution worked out first.
- `todo/tickets/for-loop-multi-param-types-unenforced.md` — declared parameter
  types on a multi-parameter loop (`-> Str $k, Int $v`) are parsed and then
  ignored. `ForLoopSpec` carries no per-parameter constraint for the VM to check.
