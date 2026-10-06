use Test;

# Found via IRC::Log::Textual: a class `multi method new` with an optional
# positional tied with an inherited/role candidate on the types. Rakudo ranks
# the candidate whose positionals are all required as narrower.
plan 5;

class A {
    multi method m(A:U: Str:D $a) { "req" }
    multi method m(A:U: Str:D $a, Int() $b = 5) { "opt" }
}
is A.m("x"), "req", 'required-only candidate wins the tie (declared first)';
is A.m("x", 2), "opt", 'the optional candidate still takes the 2-arg call';

class B {
    multi method m(B:U: Str:D $a, Int() $b = 5) { "opt" }
    multi method m(B:U: Str:D $a) { "req" }
}
is B.m("x"), "req", 'required-only candidate wins the tie (declared last)';

role R {
    proto method new(|) {*}
    multi method new(::?CLASS:U: Str:D $a) { "role" }
}
class C does R {
    multi method new(C:U: Str:D $a, Int() $b = 5) { "class" }
}
is C.new("a"), "role", 'role candidate without optionals beats the class one';
is C.new("a", 1), "class", 'two-arg call reaches the class candidate';

done-testing;
