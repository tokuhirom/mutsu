use Test;

plan 8;

# `f($_)` into a sigilless (`\v`) parameter binds the topic as rakudo does: a
# topic that is a plain value (a `for` over a List's values, a `given` over a
# computed value) is that read-only value; a topic that is a container (an
# element of a `for` over an Array or a hash's values) is aliased, so a write
# reaches the element. The positional-light call path now serves the topic
# argument itself (JSON::Fast's `jsonify($_)` inside its `for` loops took the
# general binder for every value); every answer below is rakudo 2026.11's.

sub peek(\v) { v.^name ~ ':' ~ v }
sub write(\v) { v = 'w'; 'ok' }

is (peek($_) for (1, 'a')).join(','), 'Int:1,Str:a', 'a List value topic is read as itself';
is (try { write($_) } // 'refused' for (1, 2)).join(','), 'refused,refused',
    'a List value topic is read-only';

my @a = 1, 2;
write($_) for @a;
is @a.join(','), 'w,w', 'an Array element topic is aliased';

given 42 {
    is peek($_), 'Int:42', 'a given over a value';
    is (try { write($_) } // 'refused'), 'refused', 'and that value is read-only';
}

my $x = 'orig';
with $x { is peek($_), 'Str:orig', 'a with over a variable' }

is (1, 2).map({ peek($_) }).join(','), 'Int:1,Int:2', 'a map topic';

my %h = a => 1;
write($_) for %h.values;
is %h<a>, 'w', 'a hash value topic is aliased';
