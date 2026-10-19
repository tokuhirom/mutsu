use Test;

# A routine that uses a variable the unit's top level declares only later in
# the source is a compile-time `X::Undeclared` in rakudo (#12800). mutsu
# resolved the name at run time instead, so the routine silently wrote its
# caller's same-named lexical. A compile-time error cannot be caught
# in-process, so each program runs in a child process.

plan 16;

sub run-snippet(Str:D $code) {
    my $proc = run $*EXECUTABLE.absolute, '-e', $code, :out, :err;
    ($proc.exitcode, $proc.out.slurp(:close), $proc.err.slurp(:close));
}

{
    my ($status, $out, $err) = run-snippet 'sub f { $zz = 42 }; my $zz = 1; f(); say $zz';
    isnt $status, 0, 'a sub assigning a variable declared later fails the program';
    is $out, '', '...before anything runs';
    like $err, /"Variable '\$zz' is not declared"/, '...as an undeclared variable';
}

{
    my ($status, $out, $err) = run-snippet 'class A { method m { $zz = 42 } }; my $zz = 1; A.m; say $zz';
    isnt $status, 0, 'a method using a variable declared later fails too';
    like $err, /"Variable '\$zz' is not declared"/, '...with the same message';
}

{
    my ($status, $out, $err) = run-snippet 'sub f { @later.push(1) }; my @later; f()';
    isnt $status, 0, 'an array declared later is undeclared in the routine';
    like $err, /"Variable '\@later' is not declared"/, '...naming the array';
}

# What stays legal.
{
    my ($status, $out, $err) = run-snippet 'my $x = 1; sub f { $x = 2 }; f(); say $x';
    is $out, "2\n", 'a variable declared before the routine';
}

{
    my ($status, $out, $err) = run-snippet 'sub f { my $y = 3; $y }; my $y = 1; say f()';
    is $out, "3\n", 'a routine with its own same-named lexical';
}

{
    my ($status, $out, $err) = run-snippet 'sub f($z) { $z }; my $z = 1; say f(5)';
    is $out, "5\n", 'a parameter named like a later variable';
}

{
    my ($status, $out, $err) = run-snippet 'sub f { $*dyn }; my $*dyn = 7; say f()';
    is $out, "7\n", 'a dynamic variable declared later';
}

{
    my ($status, $out, $err) = run-snippet 'sub f { $OUR::q }; our $q = 4; say f()';
    is $out, "4\n", 'a package-qualified variable';
}

{
    my ($status, $out, $err) = run-snippet '{ my $w = 2; sub g { $w }; say g() }; my $w = 5';
    is $out, "2\n", 'an enclosing block declares it before the routine';
}

{
    my ($status, $out, $err) = run-snippet 'my &h = sub { $later2 }; my $later2 = 9; say "alive"';
    isnt $status, 0, 'an anonymous sub is a routine too';
}

{
    my ($status, $out, $err) = run-snippet 'say $early; my $early = 1';
    isnt $status, 0, 'a mainline use before the declaration fails';
    like $err, /"Variable '\$early' is not declared"/, '...as an undeclared variable';
}

# vim: expandtab shiftwidth=4
