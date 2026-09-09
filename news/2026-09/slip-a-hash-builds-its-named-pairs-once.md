# Slipping a hash builds its named pairs once, and a method call stops allocating its own name

Three allocation fixes on the construction/dispatch path that [#7561](https://github.com/tokuhirom/mutsu/issues/7561)
(ADR-0019 G3) had narrowed to "diffuse cost, no single hot function". They were found with the
`alloc_scope!` accounting introduced for that ticket, extended here so the bytecode execution
itself could be attributed.

## Per-opcode allocation attribution

The ticket's remaining next step was `mfast:body` — the bytecode execution of a method body — at
40.7 allocations per method call, "entirely unexamined ... it needs sub-scoping by opcode family
before anything can be said about it". A `Scope` label has to be `&'static str`, so a new
`alloc_scope_dyn!` takes the label as an expression and `alloc_stats::opcode_label` derives (and
leaks, once per variant) the bare variant name from an `OpCode`'s `Debug` output, exactly as
`vm_stats::record_opcode` already does for the execution histogram. One call in
`exec_one_dispatch` then attributes every instruction.

Two things fall out immediately. First, with the JIT on (the default) the `op:*` rows account for
only part of the work: a hot range runs natively through `try_enter_range` and never reaches
`exec_one`, so its allocations land on the enclosing region. Attribution work on this tool wants
`MUTSU_JIT=off`. Second, with the JIT off, `benchmarks/bench-ctor.raku`'s largest exclusive regions
are not in `bless` and not in the method-body opcodes people would guess at:

| region | allocations / entry | share of program |
| --- | --- | --- |
| `op:CallMethodMut:dispatch` (exclusive) | 12.6 per method call | 28% |
| `op:MakeNamedArg` | 3.0 per named argument | 9% |
| `op:MakeSlip` | 20 per `\|%h` of 6 keys | 9% |
| `op:SetLocal` | 6.3 | 8.5% |

Finer scopes now mark the method-dispatch prologue (`cmm:decode-sources`, `cmm:names`,
`cmm:args`), the `CallMethodMut` opcode's pre/dispatch/post phases, the int-range for loop's
per-iteration bind/body/post phases, and the four phases the old `mfast:slurpy-captures-locals`
region lumped together.

## `|%h` built each named pair twice

`op:MakeSlip` was 50 allocations for a single six-key `|%_`, the largest single site in the
benchmark. `exec_make_slip_op`'s `Hash` arm called `HashData::typed_pair`, which mints the
*positional* pair flavour (`ValuePair`) with an `Arc<String>` key, and then handed it straight to
`namify_pair_item`, which — because `|%h` is always named (ADR-0021 I4) — threw that away and
rebuilt it as `Pair(String, value)`. Per entry that is a discarded `Arc<String>`, a discarded
`ValuePair`, and a second stringification of the key.

Only an object hash (`my %h{Int}`) can have a key that is not a `Str`, and that is exactly what
`HashData::has_typed_keys` reports. When it is false the result is *by construction*
`Pair(key.to_string(), value.deref_container())`, so the arm now builds that directly and leaves
the general route to object hashes. `op:MakeSlip` fell from 50 to 20 allocations per entry and the
whole program by 11.8%.

## A method call allocated its own name, twice

`cmm:names` measured 2.0 allocations on *every* `CallMethodMut` — the opcode behind a method call
on a named receiver — and both were avoidable copies of a string the constant pool already holds:

- `rewrite_method_name` returns an owned `String`, but only `.^`/`.!` actually rewrite anything;
  the sibling call paths already use a `rewrite_method_name_cow` that borrows in the common case.
- `target_name` was `const_str(..).to_string()`, immediately re-borrowed as `&target_name` at
  nearly every one of its ~29 use sites. It borrows the `CompiledCode`, not the interpreter, so it
  can just stay a `&str`.

`cmm:names` is now 0 allocations per call.

## Numbers

`MUTSU_ALLOC_STATS=1` on `benchmarks/bench-ctor.raku`, which is exact and load-independent:

| | allocations | bytes |
| --- | --- | --- |
| before (JIT on) | 1,270,755 | 78,410,380 |
| after (JIT on) | 1,120,747 | 74,262,580 |
| before (`MUTSU_JIT=off`) | 1,117,930 | 72,538,829 |
| after (`MUTSU_JIT=off`) | 1,067,941 | 72,414,957 |

Order-swapped, min-of-9 local A/B (this container, four cores, so indicative
only -- the numbers that go into documents come from the bench CI trend):
`bench-ctor` -5.4% and `bench-class` -7.5% with `MUTSU_JIT=off`, -9.1% and -2.5%
with the JIT on.

`t/slip-hash-named-args.t` pins the `|%h` semantics the fast path must preserve — named-only
binding, `Str` keys, decontainerized values, an unmodified source hash, last-wins against an
explicit named, and an object hash still reporting typed keys. All twelve assertions pass under
Rakudo too.

What #7561 still lists as open is unchanged in kind: `op:CallMethodMut:dispatch`'s remaining
exclusive cost (method resolution), `op:MakeNamedArg` and the residue of `op:MakeSlip`, both of
which are the `Value::Pair(String, ..)` key-per-pair shape already filed as
`hash-copy-allocates-a-string-per-key`, and `bless:named-args`. One guess the ticket recorded is
now settled and wrong: the locals-init loop (`mfast:loc:init`) allocates 2 times in the whole run,
not 4 per call — the private-attribute `format!` it was blamed on never runs, because such a local
resolves from the parameter list first.
