use Test;

# `@a[0][1] = v` where `@a[0]` holds a Buf must write into the Buf's own
# element storage, not autovivify a fresh Array over it. The nested-index
# assign's "does this slot already hold a container?" probe only recognized
# raw Array/Hash/ContainerRef payloads -- a Buf is a `Value::Instance` (its
# element storage lives in a shared attribute cell), so it fell through and
# got silently replaced with `[Any, 42]`.
# (todo/tickets/typed-buf-native-interop-holes.md, item 3)

plan 4;

my @a;
@a[0] = Buf[uint64].allocate(2);
@a[0][1] = 42;
is @a[0].^name, 'Buf[uint64]', 'the element is still a Buf after nested assignment';
is-deeply @a[0].list, (0, 42), 'the write landed at the right index, not the whole element replaced';

my %h;
%h<k> = Buf.new(1, 2, 3);
%h<k>[1] = 99;
is %h<k>.^name, 'Buf', 'a Buf under a hash key survives nested assignment too';
is-deeply %h<k>.list, (1, 99, 3), 'the hash-nested write landed at the right index';
