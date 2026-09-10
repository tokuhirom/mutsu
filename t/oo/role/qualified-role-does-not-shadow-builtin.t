use Test;

plan 8;

# A `role` declared with an already-qualified name registers its short name in
# the env so later code in the same module can use it bare. That must NOT
# happen when the short name is a built-in type: `role Cro::HTTP::Middleware::Pair`
# used to make every bare `Pair` in the process resolve to the user role, so
# `("a" => 1) ~~ Pair` was False and `when Pair` never matched.
role My::Deep::Pair { }
role My::Deep::Helper { }

class Uses::My::Deep::Helper does My::Deep::Helper { }

my $p = "a" => 1;

is $p.^name, 'Pair', 'a Pair is still a Pair';
ok $p ~~ Pair, 'a Pair smartmatches the built-in Pair';
ok $p.WHAT =:= Pair, 'bare Pair is the built-in type object';

my $matched = do given $p {
    when Pair { 'Pair' }
    default   { 'default' }
};
is $matched, 'Pair', 'when Pair matches a Pair';

# The same guard applies to other built-ins commonly used as a trailing name.
role Some::Where::Str { }
role Some::Where::Int { }
ok "x" ~~ Str, 'bare Str is still the built-in Str';
ok 1 ~~ Int, 'bare Int is still the built-in Int';

# The qualified role itself is unaffected by the guard.
ok My::Deep::Pair.HOW.defined, 'the qualified role is still declared';
ok Uses::My::Deep::Helper.new ~~ My::Deep::Helper, 'the qualified role still composes';
