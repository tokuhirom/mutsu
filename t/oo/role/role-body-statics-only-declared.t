use Test;

# A role body's lexicals are what the body declares -- including a phaser's
# `INIT my $x` -- and nothing else from the scope that composed it. Recording
# the composer's lexicals as role-body statics injected them into the caller
# frame of a later role-method call: a closure's own `|c` capture was
# replaced by the composer's `$c` (upstream NativeCall's replacement body).

plan 4;

role Probe[$tag] { method ping { $tag } }
multi trait_mod:<is>(Routine $r, :$probed!) { $r does Probe['p'] }
my $c = 'outer';
sub target() is probed { }
my &g = -> |c { &target.ping; c.list.elems };
is g(1, 2), 2, "a role method call keeps the caller closure's own capture";
is $c, 'outer', "and the composer's variable is untouched";

role R[::T] {
    my constant FOO = 5;
    my $x = 7;
    my sub helper() { 'h' }
    my %h = a => 1;
    INIT my $lock = 'init';
    method m { (FOO, $x, helper(), %h<a>, $lock, T.^name).join(',') }
    method m2 { (FOO, $x, helper(), %h<a>, T.^name).join(',') }
}
class K does R[Int] { }
is K.new.m, '5,7,h,1,init,Int', 'every kind of body declaration stays visible';
sub mk() { 1 but R[Str] }
is mk().m2, '5,7,h,1,Str', '... through a mixin composed in a routine';
