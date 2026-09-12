use Test;

# A subscript holds a *semilist*, so its `;` may terminate the last dimension
# rather than separate two. Data::RandomKeep writes
#
#     @!kept[ $!nb-kept < $!nb-to-keep ?? $!nb-kept++ !! $!nb-to-keep.rand; ] = ...
#
# which is the one-dimensional `@!kept[...]`, not a two-dimensional index whose
# second dimension is missing. The unconditional "parse another dimension after
# `;`" used to fail the whole statement at the closing bracket.

plan 10;

my @a = 1, 2, 3;
is @a[1;], 2, 'a trailing semicolon in a positional subscript is a terminator';
is @a[1], 2, 'and it means exactly the same as the plain subscript';

my %h;
%h{'a';} = 1;
is-deeply %h, {a => 1}, 'a trailing semicolon works in an associative subscript';
is %h{'a';}, 1, 'and reads back the same way';

# The ternary from Data::RandomKeep: an expression looser than the one the
# dimension splitter expects, terminated by the semicolon.
my @dst = 0, 0, 0;
my $flag = 0;
@dst[ $flag ?? 0 !! 1; ] = 9;
is-deeply @dst, [0, 9, 0], 'a ternary index terminated by a semicolon assigns';

# Whitespace and a newline before the closer are fine, which is how the
# distribution actually spells it.
my @nl = 1, 2, 3;
is @nl[
    2;
], 3, 'a multi-line subscript with a trailing semicolon parses';

# A real multi-dimensional subscript is untouched.
my @nested = [1, 2], [3, 4];
is @nested[1;0], 3, 'a two-dimensional subscript still indexes both dimensions';
is @nested[0;1], 2, 'and so does the other corner';

# So is a slice, and a trailing comma (which builds a one-element list, unlike
# a trailing semicolon).
is-deeply @a[1, 2], (2, 3), 'a comma slice is unchanged';
is-deeply @a[1,], (2,), 'a trailing comma still builds a one-element slice';
