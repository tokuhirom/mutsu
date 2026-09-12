use Test;

# `⚛=` is an operator, not a statement shape: it may appear anywhere an
# expression may, including a statement modifier's condition. mutsu recognised
# it only in the statement-level and parenthesized assignment parsers, so
# `return unless $!stale ⚛= 1` did not parse at all.
#
# rakudo also accepts `⚛==`: it is `infix:<⚛=>` under the assignment
# metaoperator (`&infix:«⚛==».name` answers `infix:<⚛=> + {assigning}`).
# Because `⚛=`'s result is the value it just stored, assigning that result
# back is the same store, so both spellings lower to the same thing.
# From Selkie 0.16.0 (`return unless $!mouse-capture-stale ⚛== 1`), which
# blocked Selkie, Selkie::UI and Grammar::Editor from loading.

plan 8;

my atomicint $x = 1;
is ($x ⚛= 5), 5, '⚛= in a parenthesized expression answers the stored value';
is $x, 5, '...and stored it';

$x ⚛== 7;
is $x, 7, '⚛== stores like ⚛=';
is ($x ⚛== 9), 9, '⚛== answers the stored value too';

my atomicint $flag = 0;
my $ran = 0;
$ran = 1 unless $flag ⚛= 3;
is $flag, 3, '⚛= parses as a statement modifier condition';
is $ran, 0, '...and its value is the condition';

# On an attribute, which is how Selkie writes it.
class Gate {
    has atomicint $!stale = 1;
    method check(--> Int) {
        return -1 unless $!stale ⚛== 1;
        $!stale ⚛= 0;
        $!stale;
    }
}
is Gate.new.check, 0, '⚛== / ⚛= on an attribute, in a modifier condition';

my atomicint $chain = 0;
my $got = ($chain ⚛= 2) + 1;
is $got, 3, '⚛= composes into a larger expression';
