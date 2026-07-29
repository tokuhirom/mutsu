use v6;
use Test;

# Parameterising a role compiles and runs two bodies that are not lexically the
# caller's: the type-argument expression (`R[Elem]` evaluates `Elem`) and the
# role body's deferred statements. Each publishes its value through `$_`, which
# used to land in whatever scope happened to trigger the composition — so the
# first `R[T].method` call inside a `with`/`given` block silently retopicalized
# it to the type argument, and the next `.method` ran on the wrong invocant.
#
# Found in DBIish: `LinearArray[MYSQL_BIND].new($pc)` inside
# `with $!stmt { ... }` left `$_` as `MYSQL_BIND`, so the following
# `.mysql_stmt_field_count` was dispatched on the wrong object.

plan 7;

class Elem { }
class Holder { method tag() { 'TAG' } }

role Bare[::T] { method make() { 'bare' } }
role Bodied[::T] {
    my $z = 42;
    method make() { $z }
}

with Holder.new {
    my $x = Bare[Elem].make;
    is .^name, 'Holder', 'a bare parameterised role keeps the caller topic';
    is .tag, 'TAG', 'and the next method call still dispatches on it';
}

with Holder.new {
    my $x = Bodied[Elem].make;
    is .^name, 'Holder', 'a role with a deferred body keeps the caller topic';
    is .tag, 'TAG', 'and the next method call still dispatches on it';
}

given Holder.new {
    my $x = Bare[Str].make;
    is .^name, 'Holder', 'given keeps its topic across a parameterisation';
}

for Holder.new {
    my $x = Bodied[Str].make;
    is .^name, 'Holder', 'for keeps its topic across a parameterisation';
}

# The role body's own lexicals must still survive composition — they are what
# the composed methods close over.
is Bodied[Int].make, 42, "the role body's lexical is still visible to its methods";
