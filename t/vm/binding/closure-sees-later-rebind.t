use Test;

# #9237: a closure reads the lexical's *binding*, so a `:=` rebind made after
# the closure captured the name is visible when the closure runs. A second name
# bound to the old container (`my $f := $a`, #9207) must not follow the rebind.

plan 16;

{
    my $a := [1];
    my &c = { $a };
    $a := [2];
    is-deeply c(), [2], 'mainline: closure sees a rebind to a new array';
}

sub bound-to-bound {
    my $a := 1;
    my &c = { $a };
    $a := 2;
    is c(), 2, 'sub: closure sees a rebind of a bound name';
}
bound-to-bound();

sub assigned-then-bound {
    my $a = 1;
    my &c = { $a };
    $a := 2;
    is c(), 2, 'sub: closure sees a rebind of an assigned name';
    $a := 3;
    is c(), 3, 'sub: closure sees a second rebind';
}
assigned-then-bound();

sub alias-before-capture {
    my $a = 1;
    my $f := $a;
    my &c = { $a };
    $a := 2;
    is c(), 2, 'alias bound before the capture: closure follows the rebind';
    is $f, 1, 'alias bound before the capture keeps the old container';
    $f = 7;
    is c(), 2, 'a write through the alias does not reach the rebound name';
}
alias-before-capture();

sub alias-after-capture {
    my $a = 1;
    my &c = { $a };
    my $g := $a;
    $a := 5;
    is c(), 5, 'alias bound after the capture: closure follows the rebind';
    is $g, 1, 'alias bound after the capture keeps the old container';
    $g = 9;
    is c(), 5, 'a write through that alias does not reach the rebound name';
}
alias-after-capture();

sub rebind-to-variable {
    my $a = 1;
    my $b = 10;
    my &c = { $a };
    $a := $b;
    $b = 11;
    is c(), 11, 'closure sees writes to the variable the name was rebound to';
    $a = 12;
    is $b, 12, 'a write to the rebound name reaches that variable';
    is c(), 12, 'and the closure sees it';
}
rebind-to-variable();

sub closure-writes-after-rebind {
    my $a = 1;
    my $b = 5;
    my &w = { $a = 7 };
    $a := $b;
    w();
    is $b, 7, 'a closure write after the rebind reaches the new container';
}
closure-writes-after-rebind();

{
    my $b = 1;
    my &d = sub { $b };
    $b := 3;
    is d(), 3, 'a sub closure sees the rebind too';
}

sub expression-rebind {
    my $a = 1;
    my &c = { $a };
    if $a := 42 { is c(), 42, 'an expression-level rebind is seen too' }
}
expression-rebind();
