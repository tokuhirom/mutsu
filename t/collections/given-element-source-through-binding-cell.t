use Test;

# `with %h<a><tf> { ... }` / `given %h<a><tf> { $_ = ... }` write the topic back
# to its element. When the intermediate element `%h<a>` is a shared cell (a
# `%h{$k} = ...` rewrite over a hash a sub captures), the write-back used to
# store that cell inside itself; the second call of the sub then recursed
# through the cyclic chain until the stack overflowed (LLM::Graph).

plan 7;

my %n = a => { tf => { 1 } };
for %n.kv -> $k, %v { %n{$k} = %( |%v, i => [] ); }

sub peek { with %n<a><tf> { my $x = $_ }; 9 }
is peek(), 9, 'first with over a nested element';
is peek(), 9, 'a second call survives';
is peek(), 9, 'and a third';
is %n<a>.keys.sort.join(','), 'i,tf', 'the element keeps its keys';

my %m = a => { tf => 5 };
for %m.kv -> $k, %v { %m{$k} = %( |%v, i => [] ); }
sub bump { with %m<a><tf> { $_ = $_ + 1 }; 9 }
bump() for ^4;
is %m<a><tf>, 9, 'a topic assignment still writes back through the nested element';

my %o = a => { tf => { 1 } };
for %o.kv -> $k, %v { %o{$k} = %( |%v, i => [] ); }
sub swap { with %o<a><tf> { $_ = { 2 } }; 9 }
swap() for ^3;
is %o<a><tf>.(), 2, 'replacing a Callable element writes the new one';
is %o<a>.keys.sort.join(','), 'i,tf', 'and the siblings stay';
