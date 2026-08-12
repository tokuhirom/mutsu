# Signature.gist formats the method invocant as `(C:, ...)` instead of raku's `(C $:: ...)`

Minor cosmetic divergence found in passing by the ADR-0019 E9-pre campaign (2026-08-12,
Rakudo v2026.06):

```raku
class C {
    multi method m(Int $x) { }
    multi method m(Str $x) { }
}
say C.^lookup('m').candidates.map(*.signature.gist).join(" | ");
# raku:  (C $:: Int $x, *%_) | (C $:: Str $x, *%_)
# mutsu: (C:, Int $x, *%_) | (C:, Str $x, *%_)
```

Dispatch behavior is identical (candidate order, wrap targeting — pinned by
`t/wrap-multi-candidate-scope.t`); only the invocant's textual rendering differs. raku renders
an unnamed typed invocant as `C $::` (type, anonymous scalar, `::` separator, then a space
before the first ordinary parameter) where mutsu prints `C:` and a comma. Relevant when a test
or error message compares signature strings (e.g. X::Multi::NoMatch candidate listings).

Fix in the Signature stringification path (`.gist`/`.raku` for Signature values); check
`X::Multi::NoMatch` message formatting shares the helper so both get corrected together.
