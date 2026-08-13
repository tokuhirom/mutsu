# Fixed Signature.gist rendering the method invocant as `(C:, ...)` instead of raku's `(C $:: ...)`

Found by the ADR-0019 E9-pre raku verification campaign (2026-08-12, Rakudo v2026.06):

```raku
class C {
    multi method m(Int $x) { }
    multi method m(Str $x) { }
}
say C.^lookup('m').candidates.map(*.signature.gist).join(" | ");
# raku:  (C $:: Int $x, *%_) | (C $:: Str $x, *%_)
# mutsu (before): (C:, Int $x, *%_) | (C:, Str $x, *%_)
```

Dispatch behavior was already identical (candidate order, wrap targeting — pinned by
`t/wrap-multi-candidate-scope.t`); only the invocant's textual rendering diverged. raku renders
an invocant as `Type $name::` — an anonymous `$::` when it has no explicit name (the common
implicit-`self` case), or `$name::`/`Type $name::` when the invocant is named explicitly
(`$self:`, `C $x:`) — with the trailing `::` itself standing in for the comma before the next
parameter. mutsu printed a bare `Type:` followed by a comma.

Fixed in `render_param`/`render_signature` (`src/value/signature.rs`): the invocant branch now
renders the type constraint (if any), the invocant's sigil and name, and a trailing `::`; the
signature joiner tracks when the previous rendered parameter was an invocant and emits a single
space instead of `", "` before the next one. New pin: `t/method-signature-invocant-gist.t` (6
assertions covering implicit/explicit, typed/untyped, and slurpy-following cases), verified
against Rakudo v2026.06.

While checking whether `X::Multi::NoMatch`'s candidate-signature listing shares this
rendering path, found it does not — it has its own independent formatter with three further
divergences (invocant format, a duplicate `*%_`, and a missing `:D`/`:U` smiley on positional
arg types). Filed separately as
`todo/tickets/multi-no-match-message-diverges-from-raku.md`.
