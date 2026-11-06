use Test;

# X::TypeCheck::Return names the offending value by its .raku and its class,
# as the binding errors do (Language/typesystem.rakudoc).
plan 5;

sub msg(&c) { try c(); $!.message }

class F {}
sub list-ret(--> Int) { (1, "a") }
sub hash-ret(--> Int) { %(a => 1) }
sub obj-ret(--> Int)  { F.new }
sub str-ret(--> Int)  { "x" }
sub arr-ret(--> Int)  { [1, 2] }

is msg({ list-ret }), 'Type check failed for return value; expected Int but got List ((1, "a"))', 'a List';
is msg({ hash-ret }), 'Type check failed for return value; expected Int but got Hash ({:a(1)})', 'a Hash';
is msg({ obj-ret }),  'Type check failed for return value; expected Int but got F (F.new)', 'an instance names its class';
is msg({ str-ret }),  'Type check failed for return value; expected Int but got Str ("x")', 'a Str is unchanged';
is msg({ arr-ret }),  'Type check failed for return value; expected Int but got Array ([1, 2])', 'an Array';
