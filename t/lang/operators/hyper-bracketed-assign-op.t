use Test;

# Brackets around a hyper operator's infix only group it: `»[+=]»` is
# `»+=»` and assigns back to its left side. Checked against rakudo 2026.09's
# legacy frontend: its RakuAST frontend does not compile a bracketed
# assignment infix in a hyper ("No such method 'action'").

plan 6;

{
    my @t = 1, 2;
    @t >>[+=]>> 1;
    is-deeply @t, [2, 3], '>>[+=]>> assigns to the array';
}

{
    my @t = 1, 2;
    @t «[~=]« <a b>;
    is-deeply @t, ['1a', '2b'], '«[~=]« assigns element by element';
}

{
    my @t = True, False;
    @t >>[xor=]>> True;
    is-deeply @t, [Any, True], '>>[xor=]>> assigns a word operator';
}

{
    my %h = a => 1, b => 5;
    %h >>[+=]>> 1;
    is-deeply %h, %(a => 2, b => 6), '>>[+=]>> assigns to a hash';
}

{
    my @t = 1, 2;
    is-deeply (@t >>[+]<< (3, 4)), [4, 6], '>>[+]<< still computes without assigning';
    is-deeply @t, [1, 2], 'and leaves its operand alone';
}
