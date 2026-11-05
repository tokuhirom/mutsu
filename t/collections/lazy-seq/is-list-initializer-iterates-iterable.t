use v6;
use Test;

# `my @l is List = $iterable`: List.STORE iterates a lone Iterable initializer
# (a Range, an instance of a class that does Iterable) instead of keeping it as
# one element, in statement and in expression position, and also when the
# initializer comes in a Scalar (`$*dyn`, a `my $x`). Seen in POSIX::PWDENT:
# `ok (my @db is List = $*PWDENT) ~~ List` must hold every passwd entry.

plan 8;

class Cnt does Iterable {
    method iterator {
        class :: does Iterator {
            has $.i is rw = 0;
            method pull-one { $!i < 3 ?? ++$!i !! IterationEnd }
        }.new
    }
}

my $c = Cnt.new;
my @a is List = $c;
is @a.elems, 3, 'an Iterable instance in a scalar is iterated';
is @a.raku, '(1, 2, 3)', 'and gives its items';

ok (my @b is List = $c) ~~ List, 'expression-position declaration';
is @b.elems, 3, 'also iterates';

my $r = 1..3;
my @d is List = $r;
is @d.raku, '(1, 2, 3)', 'a Range in a scalar is iterated';

my @e is List = 5;
is @e.raku, '(5,)', 'a plain value stays one element';

my @f is List = (1, 2), 3;
is @f.raku, '((1, 2), 3)', 'a list initializer keeps its items';

class Plain { }
my @g is List = Plain.new;
is @g.elems, 1, 'a non-Iterable object is one element';
