use Test;

# Path::Finder's `is constraint` trait handler re-dispatches to CORE's
# `trait_mod:<of>` on the method it was applied to.

plan 6;

class Foo { }

sub f() { 1 }
trait_mod:<of>(&f, Int);
is &f.returns.^name, 'Int', 'trait_mod:<of> sets the return type seen by .returns';
is &f.of.^name, 'Int', '... and .of';

sub g() { 1 }
trait_mod:<returns>(&g, Str);
is &g.returns.^name, 'Str', 'trait_mod:<returns> sets the return type';

sub h(--> Int) { 1 }
throws-like { trait_mod:<of>(&h, Str) }, Exception,
    message => /'Redeclaration of return type'/,
    'a routine with a declared return type cannot be redeclared';

my $seen;
multi sub trait_mod:<is>(Method $m, :$tagged!) {
    trait_mod:<of>($m, Foo:D);
    $seen = $m.returns.^name;
    $m
}
class Bar {
    method m() is tagged { Foo.new }
}
is $seen, 'Foo:D', 'a custom trait re-dispatches to trait_mod:<of> and sees the new type';
isa-ok Bar.new.m, Foo, 'the method still runs';
