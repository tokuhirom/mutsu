# Typed exceptions carry the attributes rakudo declares

Naming the class is only half of a typed exception. `throws-like 'qr/a/',
X::Obsolete, old => rx/<<qr>>/, replacement => rx/<<rx>>/` reads *attributes*,
and mutsu raised most of these classes as a bare `"X::Type: message"` string —
so the class matched, the attribute call died with
`No such method 'old' for invocant of type 'X::Obsolete'`, and the whole file
aborted at that point.

Five classes gained their attributes, each derived the way rakudo derives them
so the message and the attributes cannot disagree:

| class | attributes | raised by |
| --- | --- | --- |
| `X::Obsolete` | `.old`, `.replacement` | every Perl 5 construct the parser rejects |
| `X::Syntax::Variable::MissingInitializer` | `.type`, `.what`, `.implicit` | a `:D` declaration with no value |
| `X::Syntax::WithoutElse` | `.keyword` | `without … else/elsif/orwith` |
| `X::Comp::Trait::Scope` | `.type`, `.subtype`, `.declaring`, `.scope`, `.supported` | `is export` on a `my`-scoped variable |
| `X::Adverb` | `.unexpected`, `.what`, `.source` | an adverb `grep` does not accept |

`X::Obsolete` was the bulk of it: 20 raise sites across the parser each wrote
their own message, and none but three carried `old`/`replacement`. They now all
go through one `PError::obsolete(old, replacement)`, which builds the exception
from `RuntimeError::obsolete` — the single place the class's message shape is
spelled out — so a new obsolete-syntax rejection cannot forget the attributes.
`PError::from_typed` is the general bridge: a parse-time raise of any class
reuses the `RuntimeError` constructor rather than re-deriving the message.

Rendered messages moved closer to rakudo's as a side effect, since rakudo builds
them from the same two strings: `Unsupported use of . to concatenate strings. In
Raku please use: ~.` instead of `Perl . is dead. Please use ~ to concatenate
strings.`, and a pragma-implied `:D` now says `Variable definition of type Int:D
(implicit :D by pragma) needs to be given an initializer`.

Found under `MUTSU_REAL_TEST=1` — mutsu's native `throws-like` never called the
attribute accessors, so nothing exercised them. Five whitelisted roast files
aborted mid-plan on this and now run to completion under the real `Test`:
`S04-declarations/smiley.t`, `S04-statements/with.t`, `S11-modules/import.t`,
`S32-list/grep.t`, and `S32-exceptions/misc2.t` (which then reaches a separate
gap — `X::Syntax::Pod::BeginWithoutIdentifier` has no `.filename`, i.e. the
`X::Comp` file/line metadata, tracked in the vendoring ticket).

Pin: `t/typed-exception-attributes.t`, whose 16 assertions pass byte-identically
under `raku`.
