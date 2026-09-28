use v6;
use Test;

# `my %h is default(Nil) = a => Nil` stored `(Any)` instead of `Nil` for the
# `a` key: hash list-initialization decayed a `Nil` pair value to `Any`
# unconditionally (an untyped hash's own default) instead of consulting the
# declared `is default(...)` value. The scalar and array forms already got
# this right (#9832).

plan 4;

my %h is default(Nil) = a => Nil;
ok %h<a> === Nil, 'hash list-init with is default(Nil) keeps the declared Nil';

my %g is default(42) = a => Nil, b => 5;
is-deeply %g, { a => 42, b => 5 }, 'hash list-init decays Nil to a non-Nil declared default';

is %g<missing>, 42, 'default still applies to a missing key';

my %plain = a => Nil;
is-deeply %plain, { a => Any }, 'a hash with no is default trait still decays Nil to Any';
