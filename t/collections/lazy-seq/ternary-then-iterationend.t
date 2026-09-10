use Test;

plan 6;

# A well-known nullary sentinel term (`IterationEnd`) in the then-branch of a
# `?? ... !!` ternary was rejected as a parse error inside a routine: the ternary
# guard treats an *unknown* bareword there as a listop head that gobbled the
# `!!`, but `IterationEnd` is a complete term. P5tie's iterator does:
#   ($!lastkey := &!FIRSTKEY($!tied)) =:= Nil ?? IterationEnd !! &!mapper(...)

# IterationEnd is a valid then-branch value.
is (True ?? IterationEnd !! 5), IterationEnd, 'IterationEnd in then-branch (true)';
is (False ?? IterationEnd !! 5), 5, 'IterationEnd in then-branch (false)';

# Inside a routine (where the parse originally failed).
sub pick($cond) { $cond ?? IterationEnd !! 'other' }
is pick(True), IterationEnd, 'IterationEnd then-branch inside a sub (true)';
is pick(False), 'other', 'IterationEnd then-branch inside a sub (false)';

# Inside a class method with a private code attribute in the else-branch — the
# exact P5tie shape.
class C {
    has &!mapper = -> $x { "mapped-$x" };
    method choose($cond, $x) { $cond ?? IterationEnd !! &!mapper($x) }
}
is C.new.choose(True, 9), IterationEnd, 'method: IterationEnd then-branch (true)';
is C.new.choose(False, 9), 'mapped-9', 'method: else-branch private code attr (false)';
