use Test;

# A user-defined `trait_mod:<is>` multi typed `(Method $m, ...)` must actually
# be invoked for `method foo() is <trait>`. Real Raku modules always type the
# candidate against `Method` (an untyped `$m` doesn't even compile for a
# method-level trait: raku rejects it with "Can't use unknown trait" at the
# declaration site), so the candidate's code-object argument must itself
# report as a `Method`, not a plain `Sub`, for the signature to type-check.
# Verified against raku. See
# todo/tickets/method-typed-trait-mod-is-dispatch-never-matches.md for the
# root cause this closes.

plan 2;

my @applied;
multi sub trait_mod:<is>(Method $m, :$loud!) {
    @applied.push($m.name);
}

class Foo {
    method greet() is loud { "hi" }
}

is @applied, ("greet",), 'a Method-typed trait_mod:<is> candidate is invoked for a method trait';
is Foo.new.greet, "hi", 'the method itself still works normally';
