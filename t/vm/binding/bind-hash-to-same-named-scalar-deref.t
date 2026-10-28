use Test;

plan 3;

# Kind::Subset::Parametric (ecosystem): `my %args := %$args` (a hash bound to
# the deref of the same-named scalar) is not `my %args := %args`.
sub f($args) {
    my %args := %$args;
    %args<a>
}
is f({ a => 7 }), 7, 'same-named hash bound to a scalar deref';

sub g($m) {
    my %h := %$m;
    %h<b>
}
is g({ b => 8 }), 8, 'differently named still works';

sub h($args) {
    my @args := @$args;
    @args.elems
}
is h([1, 2, 3]), 3, 'array form with the same name';
