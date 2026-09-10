use Test;

plan 10;

# A `:=`-bound hash is marked readonly internally as a bind signal, but a
# *whole* reassignment (`%a = (...)`) must be allowed and propagate to the
# bound source — exactly like a bound array does.

{
    my %b = (a => 1);
    my %a := %b;
    %a = (z => 9);
    is %a.gist, '{z => 9}', 'whole reassign of bound hash updates %a';
    is %b.gist, '{z => 9}', 'whole reassign propagates to bound source %b';
}

# Element write on a bound hash still propagates (no regression).
{
    my %b = (a => 1);
    my %a := %b;
    %a<x> = 9;
    is %a.gist, '{a => 1, x => 9}', 'element write updates bound hash';
    is %b.gist, '{a => 1, x => 9}', 'element write propagates to bound source';
}

# `our` bound hash whole reassign.
{
    our %ob = (a => 1);
    our %oa := %ob;
    %oa = (z => 9);
    is %oa.gist, '{z => 9}', 'our bound hash whole reassign';
    is %ob.gist, '{z => 9}', 'our bound hash whole reassign propagates';
}

# A `constant` hash stays immutable — whole reassignment must die.
{
    constant %M = (a => 1);
    dies-ok { %M = (z => 9) }, 'constant hash whole reassign dies';
}

# A `constant` scalar stays immutable.
{
    constant K = 5;
    dies-ok { K = 6 }, 'constant scalar reassign dies';
}

# Bound hash in a sub does not leak its writability to a same-named outer hash.
{
    sub make-bound {
        my %b = (a => 1);
        my %a := %b;
        %a = (z => 9);
        %a.gist;
    }
    is make-bound(), '{z => 9}', 'bound hash inside sub reassigns';
    my %c = (q => 1);
    %c = (w => 2);
    is %c.gist, '{w => 2}', 'plain hash reassign still works after sub';
}
