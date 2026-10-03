use Test;

# #11115: a method call on a role type object runs the role body (the pun's
# composition) exactly once, however many calls follow; the memo probe on
# later calls must not disturb method resolution or the body's lexicals.

plan 6;

my $runs = 0;
role R {
    $runs++;
    my $greeting = 'hi';
    method m() { 1 }
    method g() { $greeting }
}

my $sum = 0;
$sum += R.m for ^500;
is $sum, 500, 'repeated punned method calls all dispatch';
is $runs, 1, 'role body ran exactly once across the calls';
is R.g, 'hi', 'body lexical still visible to a punned method';

class C does R { }
is C.m, 1, 'composing the role afterwards still works';
is C.g, 'hi', 'composed method sees the body lexical';

role S { method m() { 's' } }
is (R.m, S.m, R.m, S.m).join(','), '1,s,1,s', 'alternating punned roles resolve independently';
