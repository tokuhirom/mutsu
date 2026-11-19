use Test;

# A block's `my class` / `my role` named in a type position of a closure
# written in that block -- a variable's type constraint, a parameter type, a
# `does` trait -- names the block's type when the closure runs after the block
# exited, even where a same-named package type exists (#18177).

plan 7;

class Inner { method tag { 'outer' } }

sub a() {
    my $c;
    { my class Inner { method tag { 'block' } }; $c = { my Inner $x = Inner.new; $x.tag } }
    $c()
}
is a(), 'block', 'a variable type constraint';

sub b() {
    my $c;
    { my class Inner { method tag { 'block' } }; $c = -> Inner $x { $x.tag } }
    $c
}
my $pb = b();
dies-ok { $pb(Inner) }, 'a parameter type rejects the same-named package type';

sub c() {
    my $c;
    my $block-type;
    {
        my class Inner { method tag { 'block' } }
        $block-type = Inner;
        $c = -> Inner $x { $x.tag }
    }
    $c($block-type)
}
is c(), 'block', 'a parameter type accepts the block type';

sub d() {
    my $c;
    { my class Inner { method tag { 'block' } }; $c = -> Inner:D $x { $x.tag } }
    $c
}
dies-ok { d()(Inner.new) }, 'a parameter type with a smiley';

role R { method r { 'outer-role' } }
sub e() {
    my $c;
    { my role R { method r { 'block-role' } }; $c = { (class :: does R {}).r } }
    $c()
}
is e(), 'block-role', 'a does trait of an anonymous class';

sub f() {
    my $c;
    { my role R { method r { 'block-role' } }; $c = { my class K does R {}; K.r } }
    $c()
}
is f(), 'block-role', 'a does trait of a named class';

sub g() { { my class Inner { method tag { 'block' } }; my Inner $x = Inner.new; $x.tag } }
is g(), 'block', 'inside the declaring block itself';
