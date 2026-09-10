use Test;

# `{*}` rw-redispatch for a proto *method* (ledger §D, multi-dispatch): like the
# proto *sub* case (t/proto-rw-redispatch-coherence.t), Rakudo redispatches `{*}`
# using the proto's CURRENT (body-mutated) parameter, and a candidate's `is rw`
# write must chain back through the proto method parameter to the caller.
# Regression pin: previously the redispatch passed the entry-time value with no
# arg_source, so the candidate's `is rw` param rejected it
# ("Cannot resolve caller m(C:D: )") and the body mutation was lost.

plan 5;

# 1. proto method body mutates rw param, candidate adds to it
{
    class C1 {
        proto method m($x is rw) { $x = 30; {*} }
        multi method m(Int $x is rw) { $x = $x + 3 }
    }
    my $v = 1; C1.new.m($v);
    is $v, 33, 'proto method body mutation visible to candidate; candidate write propagates';
}

# 2. proto method param name differs from candidate (positional alias)
{
    class C2 {
        proto method m($a is rw) { $a = 10; {*} }
        multi method m(Int $b is rw) { $b = $b * 5 }
    }
    my $v = 2; C2.new.m($v);
    is $v, 50, 'redispatch aliases by position, not by name';
}

# 3. no body mutation; candidate mutates from caller value
{
    class C3 {
        proto method m($x is rw) { {*} }
        multi method m(Int $x is rw) { $x = $x + 9 }
    }
    my $v = 4; C3.new.m($v);
    is $v, 13, 'candidate rw write propagates without body mutation';
}

# 4. non-rw proto method still returns the candidate value
{
    class C4 {
        proto method m($x) { {*} }
        multi method m(Int $x) { $x * 10 }
    }
    is C4.new.m(5), 50, 'non-rw proto method dispatches to candidate';
}

# 5. is raw proto method param
{
    class C5 {
        proto method m($x is raw) { $x = 7; {*} }
        multi method m(Int $x is raw) { $x = $x + 1 }
    }
    my $v = 1; C5.new.m($v);
    is $v, 8, 'is raw proto method param chains like is rw';
}
