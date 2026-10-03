use v6;
use Test;

# A role body runs once per consuming class, and `$?CLASS` in it is that
# class -- not the class registered before it.

plan 4;

my @seen;
role R {
    my $class = $?CLASS.^name;
    @seen.push: $class;
    method composed-into() { $class }
}

class A does R { }
class B does R { }

is A.composed-into, 'A', '$?CLASS in the body is the first composing class';
is B.composed-into, 'B', '... and the second one';
is-deeply @seen, ['A', 'B'], 'the body ran once per class, in order';
ok !(R.^name eq A.composed-into eq B.composed-into), 'distinct per class';
