# Found via Markdown::Grammar t/05-Raku-section-tree: `key => @a[range]` with a
# one-element slice must keep the slice a List, not collapse to the element.
use Test;

plan 8;

my @b = 10, 20, 30;
my $i = 1;

is (k => @b[1..1]).value.WHAT.gist, '(List)', 'one-element range slice stays a List';
is (k => @b[$i..$i]).value.raku, '(20,)', 'computed one-element range slice';
is (k => @b[1,]).value.raku, '(20,)', 'one-element list slice';
is (k => @b[1..2]).value.raku, '(20, 30)', 'two-element slice';
is (k => @b[1]).value, 20, 'plain index is still the element';
is (k => @b[1..^1]).value.elems, 0, 'empty slice';

my @h = %(a => 1), %(a => 2), %(a => 3);
my @content = [0, 2, 3].rotor(2 => -1).map({ "k" => @h[$_[0] + 1 .. ($_[1] - 1)] });
sub f(@x) { @x.elems }
is f(@content[0].value), 1, 'single-element slice binds to a positional parameter';
is f(@content[1].value), 0, 'empty slice binds to a positional parameter';
