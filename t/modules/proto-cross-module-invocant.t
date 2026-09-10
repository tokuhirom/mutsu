use lib 't/lib';
use Test;

# Regression: a plain method that delegates to an attribute whose class declares a
# *cross-module* `proto method` (e.g. HTTP::Message.field -> HTTP::Header.field)
# corrupted the ENCLOSING invocant. The slow proto-dispatch interception recorded
# every changed/new env scalar as a caller-var writeback, including the caller's
# own instance variable reset to a type object, turning `$o` into `Any`.

use ProtoInvocantOuter;

plan 6;

my $o = ProtoInvocantOuter.new;

$o.poke("X");                          # sink-context delegation through the proto
ok $o.defined,               "outer invocant survives a delegated proto call (sink)";
is $o.WHAT.^name, "ProtoInvocantOuter", "outer keeps its type after the proto call";

my $p = ProtoInvocantOuter.new;
my $r = $p.poke("Y");                  # assignment context too
ok $p.defined,               "outer invocant survives a delegated proto call (assign)";

# The delegated setter still actually mutates the inner object.
my $q = ProtoInvocantOuter.new;
$q.poke-named(A => 1, B => 2);
ok $q.defined,               "outer survives a *%h delegation to the proto setter";
is $q.inner.fields.elems, 2, "inner proto setter ran and pushed both fields";
is $q.inner.field("A").value, 1, "inner proto getter reads back a stored field";
