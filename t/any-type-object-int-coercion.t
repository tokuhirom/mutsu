use Test;

plan 15;

# `.Int` on a bare Any/Mu/Cool type object (an uninitialized value) inherits
# Mu's coercion: it warns "Use of uninitialized value of type T in numeric
# context" and returns 0, rather than throwing X::Method::NotFound. This is
# the same treatment `.Numeric`/`.Real` already get
# (t/type-object-numeric-coercion.t).

is-deeply (quietly Any.Int), 0, 'Any.Int is 0';
is-deeply (quietly Mu.Int), 0, 'Mu.Int is 0';
is-deeply (quietly Cool.Int), 0, 'Cool.Int is 0';

my $undef = Any;
is-deeply (quietly $undef.Int), 0, 'variable holding Any coerces to 0 via .Int';

warns-like { my $v = Any; $v.Int }, *.contains('uninitialized' & 'numeric'),
    '.Int on an Any value warns about uninitialized value in numeric context';

# Dynamic dispatch through a type object held in a variable — DBIish's
# row-coercion pattern (`$value.$ct` in DBDish::SQLite::StatementHandle).
my $u = Any;
my $ct = Int;
is-deeply (quietly $u.$ct), 0, 'dynamic $value.$type-object .Int coercion is 0';

my class Bare {}
is-deeply (quietly Bare.Int), 0, 'user-class type object .Int is 0';

my class HasInt { method Int { 42 } }
is HasInt.Int, 42, 'user .Int method wins over the Mu default';

# Identity: Int (and its subset UInt) define `method Int { self }`.
ok (Int.Int === Int), 'Int.Int is the Int type object';
ok (UInt.Int === UInt), 'UInt.Int is the UInt type object';

# The concrete Cool types only define `Int` multis with a `:D` invocant, so
# their type objects die instead of warning.
throws-like { Num.Int }, Exception, message => /'object instance'/,
    'Num.Int dies (invocant must be concrete)';
throws-like { Str.Int }, Exception, message => /'object instance'/,
    'Str.Int dies (invocant must be concrete)';

# `Nil.Real` warns and resumes with the Int 0, like `Nil.Int`/`Nil.Numeric`
# (it used to be silently absorbed to Nil).
is-deeply (quietly Nil.Real), 0, 'Nil.Real is 0';
ok (quietly Nil.Real).defined, 'Nil.Real is defined';
warns-like { Nil.Real }, *.contains('Nil' & 'numeric'),
    'Nil.Real warns about Nil in numeric context';
