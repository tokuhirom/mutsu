# A parameter trait's `does` mixin survives to `Signature.params`

With [`does` mutating the object](does-mutates-the-object.md), a role mixed into
a materialized `Parameter` sticks to that object — but the object the trait was
handed is a throwaway. `Signature.params` has no stored `Parameter` to mix into:
`make_params_value_with_owner` maps each `SigParam` through
`sig_param_to_parameter_instance`, building a fresh `Instance` on every access,
and `check_param_custom_traits` builds its own throwaway to hand the candidate
at declaration time. So the role died with it:

```raku
role Q { }
multi trait_mod:<is>(Parameter:D $p, :$query!) { $p does Q }
sub f(:$mp is query) { }
say &f.signature.params[0] ~~ Q;   # raku: True   mutsu (before): False
```

This is the shape every `Cro::HTTP::Router` parameter trait uses (`is query`,
`is header`, `is cookie`, `is auth`), and `RouteSet!generate-route-matcher`
decides how to unpack a request by testing `$param ~~ Cro::HTTP::Router::Query`
— so without it a route declared `get -> 'search', :$term is query { }` did not
route at all.

The in-place `does` makes the cheap fix sound: the rebless is observable on the
handle `check_param_custom_traits` still holds, so after calling the candidate it
reads back the type the throwaway ended up in and records it per trait name
(`PARAM_TRAIT_MIXIN_TYPES`). Every later materialization of a parameter whose
declaration carries that trait is *born* as that type. This replays the trait's
effect on the parameter and nothing else its body did, which is exactly what the
`does`-only traits real modules write need.

One consequence had to be fixed alongside: `register_class_decl` marks every
class it registers as user-declared, which makes its collected attribute list
authoritative for accessor resolution. A synthesized `Parameter+{Query}` declares
no attributes and its built-in base contributes no declared list, so `.named`
answered `X::Method::NotFound` — `ensure_mixin_class` now gives a mixin type the
base's authority instead of its own.

`route { get -> 'search', :$term is query {...} }` compiles under mutsu now.
Stable `Parameter` identity — making `$sig.params[0] === $sig.params[0]` hold,
so a mixin applied *directly* to a materialized parameter also survives a
re-read — is still open; see
[`todo/tickets/parameter-objects-have-no-stable-identity.md`](../../todo/tickets/parameter-objects-have-no-stable-identity.md).

Pinned by `t/param-trait-mixin-persists.t` (checked against raku).
