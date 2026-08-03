use Test;

plan 11;

# Narrowness in multi dispatch is computed from the POSITIONAL parameters only.
# A named parameter's type decides whether a candidate is *applicable*, never
# how narrow it is — so candidates that differ only in their named types land in
# the same narrowness group, and rakudo resolves that by declaration order
# rather than calling it ambiguous.

{
    proto f(:$a, :$b) {*}
    multi f(Str :$a,     :$b) { "A" }
    multi f(    :$a, Str :$b) { "B" }
    multi f(Int :$a, Int :$b) { "C" }

    is f(a => "x", b => "y"), "A", 'two equally-applicable named candidates: the first declared wins';
    is f(a => 1,   b => 2),   "C", 'only one candidate applicable: it wins';
    is f(a => "x", b => 3),   "A", 'the Str :$a candidate is the only applicable one';
    is f(a => 3,   b => "y"), "B", 'the Str :$b candidate is the only applicable one';
}

# Declaration order really is what decides — reversing it reverses the answer.
{
    proto p(:$a) {*}
    multi p(Any :$a) { "Any" }
    multi p(Int :$a) { "Int" }
    is p(a => 1), "Any", 'Int :$a does NOT outrank Any :$a';
}
{
    proto q(:$a) {*}
    multi q(Int :$a) { "Int" }
    multi q(Any :$a) { "Any" }
    is q(a => 1), "Int", '...and reversing the declarations reverses the winner';
}

# A positional still narrows normally, even when a named would "disagree".
{
    proto r($x, :$a) {*}
    multi r(Any $x, Int :$a) { "Any/Int" }
    multi r(Int $x, Any :$a) { "Int/Any" }
    is r(1, a => 1), "Int/Any", 'positional narrowness decides; the named type is ignored';
}

# The positional analogue of the first case IS ambiguous, here as in rakudo.
{
    proto g($a, $b) {*}
    multi g(Str $a,     $b) { "A" }
    multi g(    $a, Str $b) { "B" }
    throws-like { g("x", "y") }, X::Multi::Ambiguous,
        'two equally-narrow POSITIONAL candidates are still ambiguous';
}

# Applicability by named type still works — an argument that no candidate's
# named type accepts is a dispatch failure, not a silent first-candidate win.
{
    proto s(:$a) {*}
    multi s(Int :$a) { "Int" }
    multi s(Str :$a) { "Str" }
    is s(a => 1),   "Int", 'named types still select by applicability (Int)';
    is s(a => "x"), "Str", 'named types still select by applicability (Str)';
}

# The shape that motivated this: HMAC's `samewith` chain, where candidates 1 and
# 2 each constrain one named and leave the other untyped.
{
    # Each `samewith` retypes one named so the next candidate takes over, exactly
    # as HMAC's `key => $key.encode` / `msg => $msg.encode` do.
    proto hm(:$key, :$msg) {*}
    multi hm(Str :$key,     :$msg) { "k:" ~ samewith key => $key.chars, :$msg }
    multi hm(    :$key, Str :$msg) { "m:" ~ samewith :$key, msg => $msg.chars }
    multi hm(Int :$key, Int :$msg) { "done" }

    is hm(key => "ab", msg => "cde"), "k:m:done",
        'candidate 1 runs, samewith reaches candidate 2, then candidate 3';
}
