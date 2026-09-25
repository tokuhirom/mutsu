use Test;

# #9330: a `}` that closes a hash subscript is not a block boundary, so the
# line-ending-block rule must not end the statement there. `.method` or an
# infix on the next line continues the expression, as in rakudo.

plan 9;

my %r = (a => 1);

my $v = %r{"a"}
    .Str;
is $v, '1', 'assignment: %h{...} newline .method chains onto the subscript';

is (%r{"a"}
    .Str), '1', 'parenthesised: %h{...} newline .method parses';

my %n = (a => { b => 2 });
is %n{"a"}{"b"}
    .succ, 3, 'nested %h{...}{...} newline .method chains';

my %m = (a => 1, b => 2);
is %m{"a", "b"}
    .elems, 2, 'slice %h{...} newline .method chains';

my %d = (x => { y => 5 });
is %d{"x";"y"}
    .Str, '5', 'multi-dim %h{a;b} newline .method chains';

is %r{}
    .elems, 1, 'zen slice %h{} newline .method chains';

my $s = %r{"a"}
    ~ "x";
is $s, '1x', '%h{...} newline infix continues the expression';

my $w = %r<a>
    .Str;
is $w, '1', 'angle subscript newline .method still chains';

# A real block-final `}` at end of line still ends the statement.
given 5 {
    my $q = do { 3 }
    .Str;
    is $q, 3, 'a block-final } at end of line still ends the statement';
}
