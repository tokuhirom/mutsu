use Test;

# #18274 slice 3: a class that composes a role is composed at BEGIN time, so
# it is compiled ahead of a destructuring declaration before it. Its methods
# still write those variables through their declaration's cells.

plan 7;

my ($general, $other) = 0, 0;
my $sentry = 0;
role Sentry { method shake() { $sentry++; 'A' } }
class Dog does Sentry {
    method shake(Str $what) {
        $general++;
        given $what {
            when 'sentry' { return self.Sentry::shake() }
        }
        'G'
    }
}

is Dog.new.shake('sentry'), 'A', 'conflict resolution dispatches to the role';
is $general, 1, 'the class method wrote the destructured variable';
is $sentry, 1, 'the role method wrote its variable';
is Dog.new.shake('x'), 'G', 'the general branch';
is $general, 2, 'and wrote it again';

my ($a, $b) = 5, 6;
BEGIN { };
role R { }
class K does R { method m { $a + $b } }
is K.new.m, 11, 'the destructured values are visible to the method';

my ($first, @rest) = 1, 2, 3;
role ArrayCaptureRole { }
class ArrayCaptureClass does ArrayCaptureRole {
    method append { @rest.push(9) }
}
ArrayCaptureClass.new.append;
is-deeply @rest, [2, 3, 9], 'the method and mainline share the destructured array';
