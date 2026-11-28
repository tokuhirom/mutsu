# `.comb(rx)` returns each matched span as a Str that shares the invocant's
# buffer (as `split` and `substr` pieces do) instead of a fresh copy. The
# pieces must still behave as independent Strs: right text over multi-byte
# characters, survivors of the invocant going away, and unaffected by later
# changes to the variable that held it. Values are rakudo's.
use Test;

plan 11;

my $s = 'user1=alpha user22=beta';
is-deeply $s.comb(/ <[a..z]>+ /).List, <user alpha user beta>, 'letter runs';
is-deeply $s.comb(/ \d+ /).List, ('1', '22'), 'digit runs';
is-deeply $s.comb(/ \d+ /, 1).List, ('1',), 'a limit';
is $s.comb(/ <[a..z]>+ /)[1].WHAT.^name, 'Str', 'each piece is a Str';
is $s.comb(/ <[a..z]>+ /)[1].chars, 5, 'with its own length';

{
    my $u = 'añb ćd éf';
    is-deeply $u.comb(/ \w+ /).List, <añb ćd éf>, 'spans over multi-byte characters';
    is $u.comb(/ \w+ /)[2].ord, 'é'.ord, 'the piece starts on the right character';
}

{
    my @pieces;
    {
        my $tmp = 'one two three';
        @pieces = $tmp.comb(/ \w+ /);
        $tmp = 'changed';
    }
    is @pieces.join('|'), 'one|two|three', 'pieces outlive and ignore the invocant variable';
    my $p = @pieces[1];
    $p ~= '!';
    is $p, 'two!', 'appending to a piece builds a new Str';
    is @pieces[1], 'two', 'and leaves the piece itself alone';
}

is 'x' x 3 ~ 'yy'.comb(/ y+ /).join, 'xxxyy', 'comb on an expression result';
