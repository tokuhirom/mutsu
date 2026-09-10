use Test;
use lib 't/lib';
use WhereConstraintMod;

# Regression pin (PLAN 8.22): calling a `where`-constrained module sub wiped
# every caller lexical declared before the call.
#
# The `where` clause check recorded the names to write back into the caller's
# local slots by diffing `env` around the clause. `Env::iter` walks only the
# innermost tier's overlay, and a nested call can flatten the parent chain into
# it mid-clause — after which every inherited caller lexical looks brand-new and
# was "written back" from its stale `env` value (the declaration seed `Any`,
# since the slot is authoritative). The compile-time `free_var_writes` of the
# clause body is the precise record and is used instead.
#
# Reproducing needs all of: the sub lives in a module, it carries a `where`
# constraint, the call's result is assigned, and the caller frame is the
# mainline (the same code inside a `{ }` block does not reproduce). Reading a
# lexical — or running any statement at all — between the declarations and the
# call also hides it, so nothing may be inserted between them below.

plan 6;

my $blob = Blob.new(1);
my $int = 99;
my $str = 'kept';
my $result = constrained(1, :c<x>);
is $result, 1, 'the constrained call returns its value';
is $int, 99, 'an Int lexical declared before the call survives';
is $str, 'kept', 'a Str lexical declared before the call survives';
is $blob.elems, 1, 'a Blob lexical declared before the call survives';

my $plain-int = 42;
my $plain-result = unconstrained(1, :c<x>);
is $plain-int, 42, 'the unconstrained control keeps its caller lexical too';
is $plain-result, 1, 'the unconstrained call returns its value';
