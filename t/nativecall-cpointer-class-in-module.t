use v6;
use lib 't/lib';
use Test;
use PgLikeNative;

# A native call returning a CPointer class that was declared inside a
# `unit module` must tag the instance with the name the class is *registered*
# under (package-qualified), so ordinary method dispatch on the handle works.
# DBDish::Pg's `PQprepare(--> PGresult)` + `$result.is-ok` is this exact shape.
plan 3;

my $h = make-handle("hello");
ok $h.defined, 'native sub returned a handle instance';
is $h.strlen, 5, 'native method on the module-scoped CPointer class works';
ok $h.is-ok, 'ordinary Raku method on the returned handle dispatches';
free-handle($h);

done-testing;
