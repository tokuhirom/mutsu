use Test;
use nqp;

# `nqp::bindattr($code, Code, '$!signature', $sig)` gives a routine a new
# signature: upstream NativeCall's `nativecast(Signature, $ptr)` builds an
# empty `sub { }` and binds the target signature onto it.

plan 6;

my $r := sub { };
nqp::bindattr($r, Code, '$!signature', :(Int $a, Str $b));
is $r.signature.arity, 2, '.signature answers the bound signature';
is-deeply $r.signature.params».name, ('$a', '$b'), '... with its parameters';
is $r.arity, 2, '.arity follows it';
is nqp::getattr($r, Code, '$!signature').arity, 2, 'nqp::getattr reads it back';

my $other := sub { };
is $other.signature.arity, 0, 'another closure of the same code keeps its own';

my $clone = $r.clone;
is $clone.signature.arity, 2, 'a clone starts from the bound signature';
