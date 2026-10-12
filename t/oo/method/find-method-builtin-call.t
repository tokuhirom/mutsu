use Test;

# Looking up and calling a core type's method object. Whether the type (or
# one of its ancestors) declares the method is answered from the catalog row,
# per ancestor, without materializing every method of the type (#12981).

plan 9;

my $chars = Str.^find_method('chars');
is $chars('abcd'), 4, 'a found Str method is callable';
is $chars.name, 'chars', 'it is named after the method';

ok Str.^find_method('uc'), 'a Str method is found';
ok Int.^find_method('abs'), 'an Int method is found';
nok Str.^find_method('no-such-method-here'), 'an unknown name is not found';

# An inherited core method: Cool declares `words`, Str inherits it.
is Cool.^lookup('words')('a b c').elems, 3, 'an inherited core method object runs';

class MyStr is Str { }
is Str.^lookup('uc')(MyStr.new(value => 'xy')), 'XY',
    'a core method object binds an instance of a user subclass';

# Repeated lookups keep answering the same way.
my $n = 0;
for ^200 { $n++ if Str.^find_method('chars')('ab') == 2 }
is $n, 200, 'repeated lookups and calls agree';

is Int.^find_method('abs')(-5), 5, 'an Int method object computes';
