# Dynamically-scoped rule parameters are established in the dynamic scope

`token value($*STOPPER = '"')` is the documented Raku idiom for parameterizing a
shared subrule from its caller — `XML::Grammar` is built on it — and on mutsu
`$*STOPPER` read back as `Nil` everywhere.

The ticket recorded that reading the parameter from *within the declaring token's
own body* worked and only a subrule call lost it. Re-measuring showed that was
wrong: nothing saw the parameter, not even its own body. This ticket and
[grammar-dynamic-rule-parameters-args-fail](grammar-dynamic-rule-parameters-args-fail.md)
turned out to be the same root cause, and are fixed together.

## Root cause

A `$*`-twigil parameter was never established in the dynamic scope at all.

mutsu turns a `token`/`rule`/`regex` body into a pattern string by evaluating it
in a throwaway scratch interpreter. Arguments were bound *there*, and the bound
values were then **textually baked** into that rule's own `{ … }` code blocks.
That is the whole of what a parameter did. Nothing was written to the interpreter
env, which is where a `$*` lookup resolves — so:

- a subrule the rule called saw nothing (the bake is per-def);
- the rule's own code blocks saw nothing either, because a block mentioning a `$*`
  variable is deferred to the reduce walk and the bake had happened on a different,
  discarded copy;
- and an argument-less call never bound anything, because a rule with no arguments
  takes the memoized static resolution path, which does not evaluate defaults.

## Fix

`src/runtime/regex/regex_dynparams.rs` establishes a rule's `$*`/`@*`/`%*`
parameters in `self.env` (the env key *is* the parameter name: `$*S` → `"*S"`) for
the duration of one invocation, and restores whatever they shadowed afterwards, so
nesting tears down correctly. The binding goes in **before** the subrule's pattern
is resolved, not just before it is matched, because the pattern may interpolate the
variable (`rule added-words { $*word $*extra }`).

The three `RegexAtom::Named` entry points (`regex_match_atom.rs`,
`regex_match_capture.rs`, `regex_match_atom_simple.rs`) each gained a thin wrapper
that performs the teardown; the inner function reports what it bound through an
out-parameter, because it can only know once the subrule's arguments have been
evaluated, and it returns from a dozen places.

A `{ … }` block that mentions a `$*` variable is always deferred to the reduce
walk (that is what makes a `:my $*x` per-match binding work), by which time the
rule that bound the parameter has long returned. So the active bindings travel
with the block in `CodeBlockContext`, exactly as its `:my` lexicals already do,
and are reinstalled around the replay.

The whole mechanism is behind a process-global flag armed at rule-registration
time, so a grammar that declares no dynamic parameter — nearly all of them — pays
one relaxed atomic load per subrule reference and nothing else.

Verified against raku: visible in the declaring rule's own body, in a directly
called subrule, two subrules down, on `token`/`rule`/`regex` alike, with an
explicitly passed argument as well as a default, correctly shadowed and torn down
across nesting levels, not leaking out of the parse, and reaching both `<?{ … }>`
code assertions and pattern interpolation. The `XML::Grammar` quoted-value shape
from the ticket now parses.

Pin: `t/grammar-dynvar-failgoal-ws.t`.
