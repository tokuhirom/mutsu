use Test;

plan 8;

# `has $.n = Nil` resets the Scalar to its default whichever constructor
# builds the object (Format::Lisp's `from-match` uses `self.bless`).
class T {
    has $.n = Nil;
    has Int $.i = Nil;
    has $.d is default(5) = Nil;
    method mk { self.bless }
}
is-deeply T.mk.n, Any, 'bless: untyped `= Nil` is Any';
is-deeply T.mk.i, Int, 'bless: typed `= Nil` is the type object';
is T.mk.d, 5, 'bless: `is default(5) = Nil` is 5';
is-deeply T.new.n, Any, 'new: untyped `= Nil` is Any';
is T.new.d, 5, 'new: `is default(5) = Nil` is 5';
ok T.mk eqv T.new, 'both constructors build the same object';

sub nothing { Nil }
class U { has $.u = nothing(); method mk { self.bless } }
is-deeply U.mk.u, Any, 'bless: an initializer evaluating to Nil';
is-deeply U.new.u, Any, 'new: an initializer evaluating to Nil';
