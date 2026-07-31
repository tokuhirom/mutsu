# A `Map` is immutable, so raku refuses every removal from one — through the
# `:delete` adverb, through `.DELETE-KEY` on the value, for a slice, and for a
# key the Map does not even hold. mutsu performed them all: nothing on any
# delete path consulted the container's immutability, though the sibling
# *assign* path already refused correctly.
#
# The refusal is a plain X::AdHoc, not one of the typed assignment exceptions
# the Set/Bag/Mix paths raise.
#
# Every assertion here also passes unmodified under rakudo.
use Test;
plan 12;

my $msg = 'Can not remove values from a Map';

# --- through the value's own protocol method ---
throws-like { Map.new("a", 1).DELETE-KEY("a") }, X::AdHoc, :message($msg),
    'Map.DELETE-KEY is refused';
throws-like { Map.new("a", 1).DELETE-KEY("zz") }, X::AdHoc, :message($msg),
    'Map.DELETE-KEY of an absent key is refused too';

# --- through the `:delete` adverb on a `%h is Map` ---
my %h is Map = a => 1, b => 2;
throws-like { %h<a>:delete }, X::AdHoc, :message($msg),
    '%h is Map: <k>:delete is refused';
throws-like { %h{'a'}:delete }, X::AdHoc, :message($msg),
    '%h is Map: {k}:delete is refused';
throws-like { %h<zz>:delete }, X::AdHoc, :message($msg),
    '%h is Map: deleting an absent key is refused';
throws-like { %h.DELETE-KEY('a') }, X::AdHoc, :message($msg),
    '%h is Map: .DELETE-KEY is refused';

# A slice delete is refused too, but the two runtimes report it differently:
# rakudo answers a List of Failures (each throws when used), mutsu throws at the
# subscript. Assert what both agree on — that nothing is removed.
try { %h<a b>:delete };
is-deeply %h.keys.sort.List, ('a', 'b'), '%h is Map: a slice delete removes nothing';

# Nothing was removed by any of the attempts above.
is-deeply %h.keys.sort.List, ('a', 'b'), 'the Map still holds both keys';

# A Capture's `.hash` is a Map too.
throws-like { \(:x(1)).hash.DELETE-KEY('x') }, X::AdHoc, :message($msg),
    'a Capture hash is a Map and refuses removal';

# --- a mutable Hash must still delete ---
my %p = a => 1, b => 2;
is (%p<a>:delete), 1, 'a plain Hash still deletes and returns the old value';
is-deeply %p.keys.List, ('b',), 'and the key is gone';
is %p.DELETE-KEY('b'), 2, 'plain Hash .DELETE-KEY still works';
