use Test;

# Pins the method-exit reconcile of a `:=`-bound attribute (reconcile_attrs):
# a ContainerRef held in a frame local must still be recovered into the
# object's attribute cell after the frame-local walk was tightened.
plan 4;

my $outer = 10;
class Bound {
    has $!x;
    method bind($y is rw) { $!x := $y; }
    method get() { $!x }
    method plain() { 1 }
}

my $b = Bound.new;
is $b.plain, 1, 'a method with no ContainerRef in scope exits normally';
$b.bind($outer);
is $b.get, 10, ':= bound attribute reads the bound value';
$outer = 11;
is $b.get, 11, 'the binding stays live past method exit';
$b.bind(my $other = 5);
is $b.get, 5, 'rebinding replaces the aliased container';
