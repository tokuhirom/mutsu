use v6;
use Test;
use lib 't/lib';
use SinkFindMethodLib;

# A statement value is sunk through its metaobject: a type with a user
# `^find_method` sees `sink` like any other method name (Object::Delayed's
# `slack { ... }` as a statement, #16331).
plan 5;

my @*SEEN;
class Bar {
    method ^find_method(Mu \type, Str:D $name) {
        return my method is-generic(--> False) { } if $name eq 'is-generic';
        my method h(|c) { @*SEEN.push("h:$name") }
        &h
    }
}
sub mk { use nqp; nqp::create(Bar) }
sub mk-raw() is raw { use nqp; nqp::create(Bar) }
my $keep = Bar;
sub keep() is raw { $keep }

mk;
is @*SEEN.join(' '), 'h:sink', 'a sunk plain call asks ^find_method for sink';
mk();
is @*SEEN.elems, 2, 'also with parentheses';
mk-raw;
is @*SEEN.elems, 3, 'an is raw routine returning a fresh object is sunk too';
keep;
is @*SEEN.elems, 3, 'an is raw routine returning a variable hands back a container, not sunk';

slack { 1 }
is @*SEEN.elems, 4, 'an imported routine is asked too';
