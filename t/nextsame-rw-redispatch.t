use Test;
plan 11;

# Function: nextsame chains rw write through to the next candidate (§D capstone).
{
    multi pr(Int $x is rw) { $x = $x + 1; nextsame }
    multi pr($x is rw)     { $x = $x + 1000 }
    my $v = 10;
    pr($v);
    is $v, 1011, 'function nextsame+rw chains both candidate writes';
}

# Function: callsame returns, then the first candidate continues and sees the
# chained value in its rw param.
{
    multi pr(Int $x is rw) { $x = $x + 1; callsame; $x = $x + 5 }
    multi pr($x is rw)     { $x = $x + 1000 }
    my $v = 10;
    pr($v);
    is $v, 1016, 'function callsame+rw continuation sees chained value';
}

# Function: three candidates chain through nextsame.
{
    multi tri(Int $x is rw where * < 100)   { $x = $x + 1;   nextsame }
    multi tri(Int $x is rw where * < 10000) { $x = $x + 100; nextsame }
    multi tri($x is rw)                     { $x = $x + 1000 }
    my $v = 5;
    tri($v);
    is $v, 1106, 'function three-candidate nextsame+rw chain';
}

# Method: nextsame chains rw write (used to die X::Parameter::RW).
{
    class C {
        multi method pr(Int $x is rw) { $x = $x + 1; nextsame }
        multi method pr($x is rw)     { $x = $x + 1000 }
    }
    my $v = 10;
    C.pr($v);
    is $v, 1011, 'method nextsame+rw chains both candidate writes';
}

# Method: callsame continuation.
{
    class D {
        multi method pr(Int $x is rw) { $x = $x + 1; callsame; $x = $x + 5 }
        multi method pr($x is rw)     { $x = $x + 1000 }
    }
    my $v = 10;
    D.pr($v);
    is $v, 1016, 'method callsame+rw continuation sees chained value';
}

# Differently named rw params still chain (source routes through first param).
{
    multi nm(Int $a is rw) { $a = $a + 1; nextsame }
    multi nm($b is rw)     { $b = $b + 1000 }
    my $v = 10;
    nm($v);
    is $v, 1011, 'differently-named rw params chain';
}

# Non-rw nextsame is unaffected (no rw_params -> original behavior).
{
    multi gr(Int $x) { 'int:' ~ $x ~ ',' ~ nextsame }
    multi gr($x)     { 'any:' ~ $x }
    is gr(5), 'any:5', 'non-rw nextsame unchanged';
}

# callwith with new args still works (non-rw).
{
    multi cw(Int $x) { 'first ' ~ callwith(20) }
    multi cw($x)     { "second:$x" }
    is cw(3), 'first second:20', 'callwith with new args unchanged';
}

# Method non-rw nextsame unchanged.
{
    class M {
        multi method foo(Int $x) { 'I' ~ $x ~ self.bar ~ nextsame }
        multi method foo($x)     { 'A' ~ $x }
        method bar { '-' }
    }
    is M.foo(7), 'A7', 'method non-rw nextsame unchanged';
}

# rw write without nextsame still propagates (single candidate path).
{
    multi solo(Int $x is rw) { $x = $x + 7 }
    my $v = 3;
    solo($v);
    is $v, 10, 'single multi candidate rw write propagates';
}

# nextsame+rw where the first candidate also reads the value back post-callsame
# into a returned expression.
{
    multi acc(Int $x is rw) { $x = $x * 2; my $r = callsame; $x = $x + $r }
    multi acc($x is rw)     { $x = $x + 100; 7 }
    my $v = 5;
    acc($v);
    is $v, 117, 'callsame return value usable alongside chained rw';
}
