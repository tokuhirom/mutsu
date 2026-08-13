# Signature.gist now renders the method invocant the way raku does

ADR-0019 E9-pre found that `Signature.gist` rendered the method invocant as `(C:, ...)` instead
of raku's `(C $:: ...)`:

```raku
class C {
    multi method m(Int $x) { }
    multi method m(Str $x) { }
}
say C.^lookup('m').candidates.map(*.signature.gist).join(" | ");
# raku:            (C $:: Int $x, *%_) | (C $:: Str $x, *%_)
# mutsu (before):  (C:, Int $x, *%_) | (C:, Str $x, *%_)
```

raku's invocant rendering is `Type $name::` — an unnamed invocant uses an anonymous `$`
(`C $::`), an explicitly named one keeps its name (`method m($self: ...)` renders `$self::`),
and the boundary is a plain space rather than a comma before the next parameter.

`render_param` (`src/value/signature.rs`) previously special-cased the invocant as just
`TypeName:`, ignoring the parameter's name entirely (mutsu already carries the distinguishing
data: an implicit invocant's `ParamDef.name` is empty, an explicit one holds the user's name).
The join loop in `render_signature` also needed to treat the invocant marker like the existing
`;;` multi-invocant separator — a space, not `", "`, follows it.

The same hand-rolled candidate-signature formatter feeding `X::Multi::NoMatch`'s "none of these
signatures matches" listing (`format_method_candidate_signatures`, `src/runtime/class.rs`) had
its own copy of the old `(Type: ...)` shape; it now builds the same `(Type $name:: ...)` prefix,
using the method's own explicit invocant param when present. Two unrelated formatting bugs
surfaced in that same error message (a duplicated `*%_` and a missing `:D`/`:U` smiley on the
argument-profile type) were left out of scope and filed separately:
`todo/tickets/nomatch-candidate-signature-slurpy-and-smiley.md`.

Pinned by `t/signature-gist-invocant-format.t` (verified against both raku and mutsu).
