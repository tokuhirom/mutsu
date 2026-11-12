use Test;

# A call's named arguments are checked against the signature: each must be
# taken by some named parameter (bare, sigiled, dynamic or a colon
# placeholder), or the call dies. The check is a prefix comparison (#15786).

plan 9;

sub a($x, :$force) { $force // "none" }
is a(1, :force), True, ':$force takes :force';
throws-like { a(1, :other) }, Exception, message => /"Unexpected named argument 'other'"/,
    'an unknown named argument dies';

sub b(:@list) { @list.elems }
is b(:list(1, 2, 3)), 3, ':@list takes :list';
sub c(:%h) { %h.elems }
is c(:h(a => 1)), 1, ':%h takes :h';
sub d(:&cb) { cb() }
is d(:cb({ 42 })), 42, ':&cb takes :cb';
sub e(:$*dyn) { $*dyn }
is e(:dyn(7)), 7, ':$*dyn takes :dyn';

is { $:name }(:name<x>), "x", '$:name placeholder takes :name';
is { @:items.elems }(:items(1, 2)), 2, '@:items placeholder takes :items';
throws-like { sub z(&cb) { }; z({ }, :cb(1)) }, Exception,
    'a positional &cb does not take :cb';
