use v6;
use Test;

# A `.`-prefixed postcircumfix inside string interpolation calls the same
# postcircumfix operator via method syntax (`$m.<x>` same as `$m<x>`,
# `%h.<a>` same as `%h<a>`, `@a.[1]` same as `@a[1]`). The interpolation
# parser accepted the bare forms but emitted the dotted forms literally
# (doc-diff sweep, Language/grammars.rakudoc:387).

plan 3;

my $m = "abc" ~~ /$<x>=b/;
is "x(\"$m.<x>\")", 'x("b")', 'dotted <> subscript on a Match interpolates';

my %h = a => 1;
is "v=%h.<a>", 'v=1', 'dotted <> subscript on a hash interpolates';

my @a = 1, 2;
is "e=@a.[1]", 'e=2', 'dotted [] subscript on an array interpolates';
