# A localized `is <trait>` word on an attribute is canonicalized

An L10N role's `trait-is2ast` map (`"lasīt-rakstīt", "rw"`) is now applied to the trait named in
`has $.x is <word>`: the attribute used to keep the localized word and die with "Can't use unknown
trait 'is' -> 'lasīt-rakstīt' in an attribute declaration". `L10N::LV`'s `t/01-latvian.t` now gets
past its class/attribute/trait test; the remaining file-wide blocker for the L10N::BG/LV/RU/UK
suites is `.DEPARSE` (ADR in progress).
