use Test;

plan 12;

# A read-modify-write of a name bound to an immutable value has no container
# to write: rakudo reports the value, "Cannot modify an immutable Str (a)",
# where a plain `=` says "Cannot assign to an immutable value" (#18986).

throws-like { my $x := "a"; $x .= uc }, X::Assignment::RO,
    message => 'Cannot modify an immutable Str (a)', '.= on a bound Str';
throws-like { my $x := 5; $x += 1 }, X::Assignment::RO,
    message => 'Cannot modify an immutable Int (5)', '+= on a bound Int';
throws-like { my $x := "a"; $x ~= "b" }, X::Assignment::RO,
    message => 'Cannot modify an immutable Str (a)', '~= on a bound Str';
throws-like { my $x := "a"; my $r = ($x .= uc) }, X::Assignment::RO,
    message => 'Cannot modify an immutable Str (a)', '.= in expression context';

try { my $x := "a"; $x .= uc }
is $!.value, 'a', 'the exception carries the value';

# A plain assignment keeps rakudo's own wording.
throws-like { my $x := "a"; $x = 1 }, X::AdHoc,
    message => 'Cannot assign to an immutable value', 'a plain = is unchanged';

# A read-only parameter keeps its wording too.
sub f($x) { $x .= uc }
throws-like { f("a") }, X::AdHoc,
    message => 'Cannot assign to a readonly variable or a value', 'a readonly parameter';

# Writable targets are unaffected.
{
    my $x = "a";
    $x .= uc;
    is $x, 'A', '.= on a container';
}
{
    my $y = "a";
    my $x := $y;
    $x .= uc;
    is $y, 'A', '.= through a name bound to a container';
}
{
    my $x = 1;
    $x += 2;
    is $x, 3, '+= on a container';
}
{
    my $x = 1;
    is ($x += 2), 3, '+= in expression context';
}
{
    class A { has $.v is rw; method m { $!v .= uc } }
    my $a = A.new(v => 'q');
    $a.m;
    is $a.v, 'Q', '.= on an attribute';
}
