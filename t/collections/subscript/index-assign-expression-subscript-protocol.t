use Test;

# An element assignment whose target is an expression -- a sub call, a method
# call, an attribute accessor -- goes through the object's own ASSIGN-POS /
# ASSIGN-KEY, as it does for a variable. It used to be dropped: the generic
# path replaced the object with a fresh aggregate nothing held (upstream
# NativeCall's `get()[2] = 7` on a `CArray[int32]`).

plan 5;

my @log;
role R {
    method ASSIGN-POS($i, $v) { @log.push: "R pos $i=$v"; $v }
    method ASSIGN-KEY($k, $v) { @log.push: "R key $k=$v"; $v }
}
class A { method ASSIGN-POS($i, $v) { @log.push: "A pos $i=$v"; $v } }

sub g() { A.new }
g()[2] = 7;
is @log.pop, 'A pos 2=7', "a sub call's result takes the class's ASSIGN-POS";

sub h() { A.new but R }
h()[3] = 8;
is @log.pop, 'R pos 3=8', "a mixed-in role's ASSIGN-POS";

h()<k> = 9;
is @log.pop, 'R key k=9', "and its ASSIGN-KEY";

class Box { has $.c }
my $b = Box.new(c => A.new but R);
$b.c[1] = 5;
is @log.pop, 'R pos 1=5', "an attribute accessor's result";

is (g()[0] = 42), 42, 'the assignment answers the assigned value';
