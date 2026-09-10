use Test;

# Iterator.skip-one / skip-at-least return an Int count (1 on success, 0 at end),
# not a Bool. Match.target returns the original string the match ran against.

plan 9;

# skip-one returns Int 1/0 (raku-oracle verified)
{
    my $i = <a b>.iterator;
    my $r = $i.skip-one;
    is $r, 1, 'skip-one returns 1 on a successful skip';
    is $r.^name, 'Int', 'skip-one result is an Int';
    is $i.pull-one, 'b', 'pull-one after skip-one yields the next element';
    is $i.skip-one, 0, 'skip-one at the end returns 0';
}

# skip-at-least returns Int 1/0
{
    my $i = <a b c>.iterator;
    is $i.skip-at-least(2), 1, 'skip-at-least(2) returns 1 when enough remain';
    is $i.pull-one, 'c', 'pull-one after skip-at-least yields the next element';
    is $i.skip-at-least(20), 0, 'skip-at-least past the end returns 0';
}

# Match.target is the original matched-against string (alias of .orig)
{
    my $m = "abc" ~~ /b/;
    is $m.target, 'abc', 'Match.target returns the original string';
    is $m.target, $m.orig, 'Match.target equals Match.orig';
}
