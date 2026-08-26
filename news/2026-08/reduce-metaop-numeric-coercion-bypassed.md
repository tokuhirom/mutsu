# The `[+]` reduce meta-operator now dispatches the numeric bridge, so it no longer silently returns `0` for objects

`[+] @objects` returned `0` for any operand that needs a `.Numeric`/`.Bridge`
method call — a user class with `method Numeric`, or a `Match` — while the three
sibling spellings all gave the right answer:

```raku
class Foo { has $.n; method Numeric { $!n } }
my @c = Foo.new(n=>2), Foo.new(n=>3);
say @c.reduce(&infix:<+>);   # 5 -- correct
say @c.reduce({$^a + $^b});  # 5 -- correct
say [+] @c;                  # 0 -- WRONG, raku: 5
```

It surfaced through the doc-diff harness on `Language/grammars.rakudoc`, where a
grammar action reduces a `Match` capture array with `[+]`/`[-]`.

## Root cause

`Interpreter::apply_reduction_op` — the table behind every reduce/hyper/zip/cross
meta-operator — is a **pure function of two `Value`s**. It has no interpreter
handle, so it cannot dispatch a user method; its internal `to_num` helper fell
through to a `0.0` default for any `Instance` it did not recognise structurally.

The plain binary `+` (and, transitively, both `.reduce` forms, which dispatch
through the ordinary operator) routes its operands through
`coerce_numeric_bridge_pair` first. Only the compiled meta-operator went
straight to the pure table.

## Fix

`eval_reduction_operator_values` — which already has `&mut self`, and already
takes first refusal for `Junction` operands just above — now also takes first
refusal when the operator is genuinely numeric and either operand is an object
or a container cell. It tries a user `infix:<op>` candidate first (so a
class that overloads the operator wins, as multi-dispatch would), then falls
back to the same numeric bridge the binary operator uses.

## The same gap on the string side

`apply_reduction_op`'s *string* arms have exactly the same limitation, and it
reproduced just as cleanly:

```raku
class S { has $.s; method Str { $!s } }
my $a = S.new(s => "ab"); my $c = S.new(s => "c");
say $a ~ $c;      # abc   -- binary ~ dispatches the user Str
say [~] $a, $c;   # S()S() -- WRONG, the table only reached .gist
say [lt] $a, $c;  # False  -- WRONG, it compared those renderings
```

Fixed in the same pass, through `coerce_stringy_operand` — the helper infix `~`
and the string comparisons already share — for `~ ~| ~^ ~& eq ne lt gt le ge
leg`. Leaving it would have made the reduce fix half-done for the same root
cause.

Two details worth keeping:

* The operator set is restricted to the arms that actually numify
  (`+ - * / div % mod ** +& +| +^ +< +> gcd lcm` and the numeric comparisons).
  The `cmp`-flavoured comparators (`cmp`, `leg`, `min`, `max`, `minmax`,
  `before`/`after`) are deliberately excluded: rakudo orders those with `cmp`
  semantics rather than numification, so coercing their operands would change
  what they compare.
* A `ContainerRef` operand is bridged for the same reason PR #6999 gave for the
  comparison operators — an aliased cell numified to `0` instead of reading
  through to the value it holds.

Pinned by `t/numeric-coercion-gaps.t` (`[+]`/`[*]`/`[-]`/`[<]` over objects, a
`[+]` over `Match` captures, `[+]` mixing an object with a plain `Int`, and the
plain-value and `[~]`/`[max]` cases that must be unaffected).
