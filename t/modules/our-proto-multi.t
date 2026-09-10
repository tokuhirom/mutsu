use v6;
use Test;

# `our multi sub` candidates are legal when an `our proto` for the same name is
# declared in the same scope (rakudo: "Cannot use 'our' with individual multi
# candidates. Please declare an our-scoped proto instead"). Regression: mutsu
# hoists sub declarations (including multi candidates) before the in-sequence
# `our proto` registration ran, so the scope check fired against an empty proto
# table and rejected valid `our proto` + `our multi` groups (real dists such as
# JavaScript::Google::Charts declare their API this way).

plan 5;

our proto sub gen($data, :$version) {*}
our multi sub gen(Int $data, :$version = 'row') { "int:$version" }
our multi sub gen(Str $data, :$version = 'col') { "str:$version" }

is gen(1), 'int:row', 'our multi Int candidate dispatches';
is gen('x'), 'str:col', 'our multi Str candidate dispatches';
is gen('x', :version<z>), 'str:z', 'named arg reaches the our multi candidate';

# proto declared textually before the candidates, candidates before use.
is gen(42, :version<v>), 'int:v', 'first candidate honours the passed named arg';

# An `our multi` with no proto at all is still rejected.
throws-like 'our multi sub nope($x) { $x }', X::Declaration::Scope::Multi,
    'our multi without an our proto is rejected';
