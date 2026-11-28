use Test;

# #18267: a method-call site remembers the plan or accessor read that last
# answered it (`vm_method_site_ic`). A hit must re-check everything the
# answer depends on: the receiver's class, the argument types, and every
# registry change (a new method, a wrap, `^add_method`, `augment`).

plan 13;

class A {
    has $.x = 1;
    method m($n) { $n + $!x }
    method z() { 'A.z' }
}
class B is A {
    method m($n) { $n * 10 }
}

sub call-m($o, $n) { $o.m($n) }
sub call-z($o) { $o.z }
sub read-x($o) { $o.x }

my @r;
for ^5 { @r.push: call-m(A.new, 1) }
is-deeply @r, [2 xx 5], 'a warm site';
is call-m(B.new, 1), 10, 'another class at the same site';
is call-m(A.new, 1), 2, 'and back';
is call-m(A.new, 1.5), 2.5, 'another argument type at the same site';
is call-m(A.new(x => 5), 1), 6, 'another instance of the same class';

for ^5 { read-x(A.new) }
is read-x(A.new(x => 7)), 7, 'a warm accessor site';
is read-x(B.new(x => 8)), 8, 'the accessor site for a subclass';

for ^5 { call-z(A.new) }
A.^add_method('z2', method () { 'z2' });
is call-z(A.new), 'A.z', 'a site survives an unrelated ^add_method';

&call-z.wrap(-> $o { 'wrapped:' ~ callsame });
is call-z(A.new), 'wrapped:A.z', 'a wrapped caller';

class C { has $.v = 3; method v() { 'own:' ~ $!v } }
for ^5 { read-x(A.new) }
is read-x(A.new(x => 9)), 9, 'accessor site still right after new classes';

use MONKEY-TYPING;
class D { method d() { 'old' } }
class D2 is D { }
sub call-d($o) { $o.d }
for ^5 { call-d(D2.new) }
augment class D2 { method d() { 'new' } }
is call-d(D2.new), 'new', 'a site sees a method augmented into the subclass';

{
    my @t = await (^4).map: -> $t {
        start { my $s = 0; $s += call-m(A.new, $_) for ^200; $s }
    };
    is-deeply @t, [(^200).sum + 200 xx 4], 'concurrent calls through warm sites';
}

{
    my $o = A.new;
    my $r = try { call-m($o, 'abc') };
    ok $! ~~ X::Str::Numeric, 'an exception through a warm site';
}
