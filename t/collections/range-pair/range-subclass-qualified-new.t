use Test;

plan 5;

# Math::Sequences (ecosystem): `self.Range::new($min, $max, :$excludes-min, ...)`
# from the `new` of a `class ... is Range` died "No such method 'new' for
# invocant of type 'Range'"; the result is a range of the subclass.
class Ints is Range {
    method new($min = 1, $max = Inf, :$excludes-min, :$excludes-max) {
        self.Range::new($min, $max, :$excludes-min, :$excludes-max);
    }
    method tag { 'ints' }
}

my $r = Ints.new(2, 5);
isa-ok $r, Ints, 'a subclass instance from the type object';
is $r.min, 2, 'min';
is $r.max, 5, 'max';
is $r.tag, 'ints', 'its own methods';

class Inst is Range {
    method again { self.Range::new(7, 9) }
}
is Inst.new(1, 2).again.max, 9, 'from an existing instance';
