use Test;

# A sub nested in a routine and called before its declaration shares the
# routine's own `@`/`%` variables from the routine's entry, even when a
# program-scope sub of the same name has an identical body and so the same
# compiled code (#13890). Checked against rakudo.

plan 8;

my @u = 'main';
my %h = a => 1;
sub acc { @u.join }
sub hk { %h.keys.sort.join }
is acc() ~ hk(), 'maina', 'the program-scope subs read the program variables';

sub mk2 {
    my @u = 'mk2'; my %h = b => 2;
    my $r = acc() ~ hk();
    sub acc { @u.join }
    sub hk { %h.keys.sort.join }
    $r
}
is mk2(), 'mk2b', 'identical nested bodies read the routine variables';

sub early { my $r = acc2(); my @v = 1, 2; sub acc2 { @v.elems }; $r ~ '/' ~ acc2() }
is early(), '0/2', 'a call before the declaration sees the empty array';

sub twice { my @w; push2(); push2(); sub push2 { @w.push(1) }; @w.elems }
is twice(), 2, 'pushes through the nested sub reach the routine array';
is twice(), 2, '...afresh on each call';

sub t3 { my @w = (); push3(); sub push3 { @w.push(1) }; @w.elems }
is t3(), 1, 'with an initializer';

sub f { my Int @w; g(); sub g { @w.push('x') } }
throws-like { f() }, X::TypeCheck, 'the declared element type still applies';

sub m { my Int @w = 1; n(); sub n { @w.push(2) }; @w.join(',') }
is m(), '1,2', 'a typed array shares its container';
