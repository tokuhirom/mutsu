# A compound sibling class no longer captures a bare type in a role

In `unit module PP`, `role Type { multi method decode(Int, Blob:D $b) {...} }` followed by
`class Type::Blob does Type` had the role's `Blob:D` rewritten to `PP::Type::Blob:D` by the
parser's type-qualification pass, which treated the compound declaration `Type::Blob` as a type
declared inside package `Type`. A compound name lives in that stash but binds no lexical there, so
the pass now ignores it. The real `Protocol::Postgres` loads again.
