use Test;

# Found via the Math::Matrix distribution (t/022-converter.rakutest): in a
# paren-less call to an imported routine, `|$x == EXPR` slips only `$x`.

plan 4;

class Rows {
    has @.r = 1, 2, 3, 4;
    method list { @!r.list }
}
my $m = Rows.new;
my @a = 1, 2, 3, 4;

ok |$m == (1, 2, 3, 4), 'slipped object compared to a list';
ok |$m == (1, 2, 3, 4), 'with a description';
ok |@a == (1, 2, 3, 4), 'slipped array compared to a list';
is-deeply [(|@a, 5)], [1, 2, 3, 4, 5], 'a lone |@a argument still slips';
