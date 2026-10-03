use Test;

# Assigning to a typed hash from inside a closure keeps the hash's `Hash[T]`
# type, as it already did for `@` arrays. Found in Data::Reshapers:
# `my Hash %res; lives-ok { %res = cross-tabulate(...) }; %res.isa(Hash[Hash])`.

plan 5;

sub call(&c) { c() }

my Hash %r;
call({ %r = a => %(x => 1) });
is %r.WHAT.raku, 'Hash[Hash]', 'Hash[Hash] survives a closure assignment';

my Int %i;
my &cl = { %i = a => 1 };
cl();
is %i.WHAT.raku, 'Hash[Int]', 'Hash[Int] survives a stored closure';

my %oh{Int};
call({ %oh = 1 => 2 });
is %oh.keys[0].WHAT.raku, 'Int', 'an object hash keeps its typed keys';

my Int %bad;
throws-like { call({ %bad = a => 'x' }) }, X::TypeCheck,
    'element type is still checked';

my Int @a;
call({ @a = 1, 2 });
is @a.WHAT.raku, 'Array[Int]', 'arrays keep their type too';
