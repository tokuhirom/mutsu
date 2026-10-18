use Test;

# A sub nested in a routine and called before its declaration shares the
# routine's own scalar from the routine's entry, even when a program-scope sub
# of the same name has an identical body and so the same compiled code
# (#13900). The shared container still answers as the untyped scalar it is
# declared as. Checked against rakudo.

plan 9;

my $s = 'main-s';
sub sv { $s }
is sv(), 'main-s', 'the program-scope sub reads the program variable';

sub mk2 { my $s = 'mk2-s'; my $r = sv(); sub sv { $s }; $r }
is mk2(), 'mk2-s', 'the identical nested body reads the routine variable';
is mk2(), 'mk2-s', '...on each call';
is sv(), 'main-s', 'the program-scope sub still reads the program variable';

sub early { my $r = sv2() // 'undef'; my $v = 5; sub sv2 { $v }; $r ~ '/' ~ sv2() }
is early(), 'undef/5', 'a call before the declaration sees an undefined scalar';

sub bump { my $n = 0; inc(); inc(); sub inc { $n++ }; $n }
is bump(), 2, 'writes through the nested sub reach the routine scalar';
is bump(), 2, '...afresh on each call';

sub typed { my Int $t = 1; set-t(); sub set-t { $t = 'x' } }
throws-like { typed() }, X::TypeCheck, 'the declared type still applies';

{
    sub bump-rw($p is rw) { $p⚛++ }
    my $plain = 1;
    my $alias := $plain;
}
{
    module NestedSubOwnScalar {
        my $plain = 0;
        sub bump-rw($p is rw) { $p⚛++ }
        our sub bump-plain() { bump-rw($plain) }
    }
    throws-like { NestedSubOwnScalar::bump-plain() }, X::Multi::NoMatch,
        'an integer atomic still refuses the untyped scalar';
}
await start { 1 };
