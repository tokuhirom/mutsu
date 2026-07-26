use Test;

plan 8;

# A space-separated adverb binds to the method call it follows; the comma after
# it continues the *enclosing* listop's argument list. Only the colon-call form
# (`.m: a, b`) takes the comma list itself.
#
# `is-deeply $sth.row :hash, %want, 'desc'` used to hand `.row` the two
# following arguments, so a zero-positional signature reported
# "Too many positionals passed" (DBIish t/05-mock.rakutest).

class Row {
    method row(:$hash, :$other) {
        ($hash ?? 'H' !! '-') ~ ($other ?? 'O' !! '-');
    }
    method args(*@a) { @a.join(',') }
}

my $r = Row.new;
sub show(*@a, *%n) { '[' ~ @a.join('|') ~ ']' ~ %n.keys.sort.join(',') }

is show($r.row(:hash), 'x', 'y'), '[H-|x|y]',
    'the parenthesised adverb is the control';
is show($r.row :hash, 'x', 'y'), '[H-|x|y]',
    'a bare adverb leaves the following list to the listop';
is show($r.row :hash :other, 'x'), '[HO|x]',
    'space-separated adverbs all bind to the method call';
is show($r.row :hash, :other, 'x'), '[H-|x]other',
    'an adverb after a comma belongs to the enclosing call';
is show($r.row :hash), '[H-]',
    'a bare adverb with nothing after it still binds';

# The colon-call form is unchanged: it still takes the whole comma list.
my $joined = $r.args: 1, 2, 3;
is $joined, '1,2,3', 'the colon-call form still takes the whole comma list';
is show($r.args: 1, 2), '[1,2]', 'a colon call nested in a listop';

# A hyper method call follows the same rule.
my @rows = Row.new, Row.new;
is show(@rows>>.row :hash, 'z'), '[H-|H-|z]',
    'a bare adverb on a hyper method call leaves the list to the listop';
