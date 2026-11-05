use Test;

# `defined EXPR` tests the value an explicit `.AT-KEY(...)` call reads, not the
# lazily autovivifying element slot of a missing key (#16080). Checked against
# rakudo.

plan 9;

my %h = a => 1, b => Int;
my @a;

is (defined %h.AT-KEY('missing')), False, 'a missing key is not defined';
is (defined %h.AT-KEY('a')), True, 'a key holding a value is defined';
is (defined %h.AT-KEY('b')), False, 'a key holding a type object is not defined';
is (defined %h<missing>), False, 'the subscript form agrees';
is %h.AT-KEY('missing').defined, False, 'so does the method form';
is (defined @a.AT-POS(3)), False, 'an out-of-range AT-POS agrees';

class Cache {
    has %.store;
    method get($key) is raw { %!store.AT-KEY($key) }
}
my $c = Cache.new;
is (defined $c.get('x')), False, 'through an is-raw wrapper method';
$c.store<x> = 5;
is (defined $c.get('x')), True, '...and defined once stored';
ok !(defined %h.AT-KEY('missing')), 'usable as a negated condition';
