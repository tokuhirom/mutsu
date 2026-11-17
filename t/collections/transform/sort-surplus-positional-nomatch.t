use Test;

# `.sort` binds at most one positional, and it must be Callable (#17830):
# anything else is an X::Multi::NoMatch listing rakudo's candidates.

plan 12;

sub failure(&code) {
    code();
    CATCH { default { return ($_.^name, $_.message) } }
    ('none', '');
}

{
    my ($name, $msg) = failure { (1, 2).sort(1, 2) };
    is $name, 'X::Multi::NoMatch', 'two positionals';
    is $msg.lines[0], 'Cannot resolve caller sort(List:D: Int:D, Int:D); none of these signatures matches:',
        'the header names the call profile';
    is $msg.lines.elems, 7, 'Any and List candidates are listed';
}

{
    my ($name, $msg) = failure { (3, 1).sort(1) };
    is $name, 'X::Multi::NoMatch', 'a non-Callable positional';
    is $msg.lines[0], 'Cannot resolve caller sort(List:D: Int:D); none of these signatures matches:',
        'names the Int argument';
}

{
    my ($name, $msg) = failure { (3, 1).sort(1, :k) };
    is $msg.lines[0], 'Cannot resolve caller sort(List:D: Int:D, :k); none of these signatures matches:',
        'a True Bool named argument reads as its colonpair';
}

{
    my ($name, $msg) = failure { my %h = a => 1; %h.sort(1) };
    is $msg.lines[0], 'Cannot resolve caller sort(Hash:D: Int:D); none of these signatures matches:',
        'a Hash invocant';
    is $msg.lines[*-1], '    (Map:D $:: *%_ --> Seq:D)', 'lists the Map candidate';
}

{
    my ($name, $msg) = failure { (1..3).sort(1) };
    is $msg.lines.elems, 4, 'a Range lists only the Any candidates';
}

{
    my ($name, $msg) = failure { (1, 2).Seq.sort(*) };
    is $msg.lines[0], 'Cannot resolve caller sort(List:D: Whatever:D); none of these signatures matches:',
        'a Seq redispatches to List.sort';
}

is-deeply (3, 1, 2).sort(&infix:<cmp>).list, (1, 2, 3), 'a Callable comparator still sorts';
is-deeply (3, 1, 2).sort(-*).list, (3, 2, 1), 'a WhateverCode mapper still sorts';
