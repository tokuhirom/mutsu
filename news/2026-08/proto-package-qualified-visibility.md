# A package-qualified call to a `my`-scoped proto/multi no longer bypasses visibility

```raku
# inside module PrivateProtoModule:
proto sub secret(|) is export {*}
multi sub secret(Int $x) { "int:$x" }

# from a consumer:
use PrivateProtoModule;
secret(5);                          # "int:5" — fine, exported under the short name
PrivateProtoModule::secret(5);      # raku: dies "Could not find symbol '&secret' in 'PrivateProtoModule'"
                                     # mutsu (before this fix): "int:5" — wrongly succeeded
```

Found while investigating
`todo/tickets/package-qualified-proto-dispatch-blocks-gather-forcing.md`. That
ticket's own repro sketch (a package-qualified call to a *private, non-`our`*
proto reportedly *failing* where an unqualified in-module call succeeds)
turned out not to reproduce — the ticket had already flagged itself as
possibly stale ("Not independently re-verified... whoever picks this up
should re-derive a precise, currently-reproducing repro first"). Building a
careful, minimal repro from scratch instead found mutsu diverges from real
Rakudo in the OPPOSITE direction for this shape: mutsu was too *permissive*,
letting a package-qualified call through where Rakudo correctly rejects it.

A plain (non-multi) `my`-scoped sub already had this right — `Pkg::plainSub(...)`
correctly died with the same message Rakudo uses. Only the proto/multi
resolution path skipped the check.

## Root cause

`resolve_proto_function` (`src/runtime/dispatch_proto.rs`), for a
package-qualified name (`name.contains("::")`), looked the name up directly
in `registry().proto_functions` with no visibility gate at all. The plain-sub
resolution path (`dispatch_resolve.rs`) already has exactly the right gate —
`qualified_name_hidden_here`, backed by `my_scoped_package_items`/
`our_scoped_package_items` (a `my`-scoped, i.e. non-`our`, package item is
exported and callable under its short name but is never a real package-stash
symbol) — but `resolve_proto_function` is a separate resolver that never
consulted it.

## Fix

`resolve_proto_function`'s qualified-name branch now checks
`qualified_name_hidden_here(name)` first, mirroring the plain-sub gate
exactly. An `our proto sub`/`our multi sub` is unaffected (it's genuinely a
package symbol, so the gate correctly lets it through); only the `my`-scoped
(default) case now dies like Rakudo.

Regression tests: `t/proto-package-qualified-visibility.t` (with a new
fixture, `t/lib/PrivateProtoModule.rakumod`), verified against real `raku`.

## What this means for the original ticket

The ticket's two linked symptoms (a failing package-qualified proto call, and
a `gather`/`samewith` forcing failure downstream of it) remain unverified —
this session's repro attempts found mutsu too lenient here, not too strict,
which is a different bug in the same code area, not confirmation of either
original symptom. The original ticket is retired as unreproducible-as-written;
if the `gather`/`samewith` cross-module-scope symptom is independently
confirmed later, it should be re-filed with its own fresh repro (it does not
depend on this fix — the visibility gate this fix adds only tightens an
already-succeeding call path, it does not touch dispatch of a genuinely
in-scope proto).
