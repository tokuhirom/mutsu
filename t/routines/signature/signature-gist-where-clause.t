use Test;

# Signature.gist / .raku print a `where` clause as `where { ... }`, never as
# its source and never omitted (Test::Mock's "called ... with arguments
# matching :($ where { ... })" messages depend on it).
plan 10;

is :($ where { $_ > 1 }).gist, '($ where { ... })', "anonymous scalar with a block constraint";
is :($x where * > 1).gist, '($x where { ... })', "named scalar with a WhateverCode constraint";
is :(Int $x where { $_ > 1 }, Str $y where /a/).gist,
    '(Int $x where { ... }, Str $y where { ... })', "two constrained parameters";
is :(Int $ where 1..5).gist, '(Int $ where { ... })', "a range constraint";
is :($ where Int).gist, '($ where { ... })', "a type constraint";

is :(Int $x where { $_ > 1 } = 5).gist, '(Int $x where { ... } = 5)', "where before the default";
is :(Int :$x! where { 1 }).gist, '(Int :$x! where { ... })', "required named";
is :(Int $x? where { 1 }).gist, '(Int $x? where { ... })', "optional positional";
is :($x is copy where { 1 }).gist, '($x is copy where { ... })', "after the traits";

sub f(Int $x where * > 1, :$n where { 1 }) { }
is &f.signature.raku, ':(Int $x where { ... }, :$n where { ... })', ".raku of a declared signature";
