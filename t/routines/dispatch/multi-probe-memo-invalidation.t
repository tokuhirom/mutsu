use Test;
plan 10;

# A user multi that shadows a builtin for some argument types: the probes behind
# `user_function_matches_call` (`has_multi_function`, `has_multi_candidates`) are
# memoised under the function-table generation and the name's family generation.
# Each assertion below changes the family (or re-dispatches) between two calls of
# the same name, so a stale memo shows up as the old answer.
multi sub uc(Int $n) { "int:$n" }
is uc(5), 'int:5', 'a user multi candidate answers its own types';
is uc("a"), 'A', 'another type still reaches the builtin';

{
    multi sub uc(Str $s) { "mine:$s" }
    is uc("a"), 'mine:a', 'a candidate added in a nested scope is seen at once';
    is uc(5), 'int:5', 'the outer candidate is still seen';
}
is uc("a"), 'A', 'the scope exit takes the candidate away again';

# A re-dispatch (`callsame` / `nextwith` / `samewith`) runs more candidates but
# must not freeze the family: a candidate declared after it is visible.
multi sub gr(Int $n) { "int" }
multi sub gr(Any $x) { "any>" ~ callsame() ~ "<" }
multi sub gr(Mu $x)  { "mu" }
is gr(1), 'int', 'narrowest candidate wins';
is gr("s"), 'any>mu<', 'callsame reaches the next candidate';
{
    multi sub gr(Str $s) { "str>" ~ callsame() ~ "<" }
    is gr("s"), 'str>any>mu<<', 'a candidate added in a nested scope joins the re-dispatch chain';
}
is gr("s"), 'any>mu<', 'and leaves it again with the scope';
is gr(2.5), 'any>mu<', 'an unrelated type is unchanged';
