use Test;

# `$<>` / `$/<>` is the zen slice of `$/`: the Match itself, not its string
# form, so it can be subscripted (found via the DB::ORM::Quicky distribution,
# which writes `s/ ... (.*) ... /$<>[0]/`).

plan 9;

"abc" ~~ /(b)(c)/;
isa-ok $<>, Match, '$<> is the Match';
isa-ok $/<>, Match, '$/<> is the Match';
is $<>[0], 'b', '$<>[0] indexes the Match';
is $<>[1], 'c', '$<>[1] indexes the Match';
is $<>.elems, 2, '$<>.elems counts positional captures';

my $s = 'CREATE TABLE t ( a int, b text )';
$s ~~ s/ ^ .*? '(' (.*) ')' .*? $ /$<>[0]/;
is $s, ' a int, b text ', '$<>[0] in an s/// replacement';

"abc" ~~ /(b)(c)/;
is "x$<>[0]y", 'xby', '"$<>[0]" interpolates the capture';
is "x$<>y", 'xbcy', '"$<>" interpolates the whole match';
is "x$<>[1]y", 'xcy', '"$<>[1]" interpolates the second capture';
