# A `does` mixin applied by a parameter trait does not persist

`multi trait_mod:<is>(Parameter:D $param, :$query!) { $param does Query }` now
runs at declaration time (see `news/2026-08/custom-parameter-traits.md`), but the
role it mixes in is thrown away. Reading the parameter back gives a plain
`Parameter`:

```raku
role Q { }
multi trait_mod:<is>(Parameter:D $p, :$query!) { $p does Q }
sub f(:$mp is query) { }
say &f.signature.params[0].^name;      # raku: Parameter+{Q}   mutsu: Parameter
say &f.signature.params[0] ~~ Q;       # raku: True            mutsu: False
```

Mixing in directly has the same result, so the gap is not specific to traits:

```raku
role Q { }
sub f(:$mp) { }
my $p = &f.signature.params[0];
$p does Q;
say $p.^name;   # raku: Parameter+{Q}   mutsu: Parameter.new
say $p ~~ Q;    # raku: True            mutsu: False
```

## Root cause

There is no stored `Parameter` object to mix into. `Signature.params` is built
by `make_params_value_with_owner` (`src/value/signature.rs`), which maps each
`SigParam` through `sig_param_to_parameter_instance` — a fresh `Instance` with
attributes derived from the `ParamDef`, constructed on every access. A mixin
applied to one of those instances dies with it. The declaration-time trait
dispatch added in `check_param_custom_traits`
(`src/vm/vm_register_sub_ops.rs`) has the same problem from the other side: it
builds a throwaway `Parameter` to hand to the candidate.

## Why it matters

`Cro::HTTP::Router::RouteSet!generate-route-matcher` walks `$sig.params` and
tests `$param ~~ Cro::HTTP::Router::Query` (and `::Header`, `::Cookie`,
`::Auth`) to decide how to unpack a request. With the mixin dropped, every such
parameter looks like an ordinary named parameter, so a route declared
`get -> 'search', :$min-price is query = 0 { }` does not route. That is the
remaining blocker on `http-router.rakutest`, `router-auth.rakutest` and
`http-router-named-urls.t` after the parse fix.

## Sketch of a fix

Either (a) record the applied roles back onto the `SigParam` at declaration time
and re-mix them whenever a `Parameter` instance is materialized — cheap, and
correct for the `does`-only traits real modules write, but it re-runs nothing
else the trait body did; or (b) materialize each signature's `Parameter` objects
once, cache them alongside the registered `SigInfo` (`register_sig_info` already
keys on the Signature instance id), and hand out the cached values so a mixin
sticks. (b) is the honest version and also fixes the direct-`does` case above.

Note that `.signature` itself is re-derived in several places; whichever route
is taken, the identity of `Parameter` objects has to become stable enough that
`$sig.params[0] === $sig.params[0]` holds, which it currently does not.
