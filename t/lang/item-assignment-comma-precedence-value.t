use Test;

plan 6;

# Item assignment binds tighter than `,`, so `my $d = 6, 3` is
# `(my $d = 6), 3`: `$d` gets 6, and the statement's value is the whole list.
# A routine ending in it returns `(6 3)` (Math::Handy's `infix:</%>` is
# `{ my $div = ($n / $m).Int, $n % $m }`); mutsu returned only the last item.

sub divmod($n, $m) { my $div = ($n / $m).Int, $n % $m }
is-deeply divmod(33, 5), (6, 3), 'routine returns the whole comma list';

sub three { my $d = 6, 3, 4 }
is-deeply three(), (6, 3, 4), 'every trailing item is in the value';

sub guarded { my $x = 1, 2 if True }
is-deeply guarded(), (1, 2), 'with a statement modifier';

my $c = 1, 2;
is $c, 1, 'the scalar still gets only the first item';

sub later { my $d = 6, 3; $d + 1 }
is later(), 7, 'a following statement sees the declared scalar';

my $warned = False;
{
    CONTROL { when CX::Warn { $warned = True if .message.contains('$d'); .resume } }
    my $d = 6, 3;
}
nok $warned, 'no sink warning names the declared scalar';
