use v6;
use Test;

# A closure stored into a HASH ELEMENT (`%h<key> = sub {...}`) escapes its
# creating frame exactly like one passed as a named call argument -- but
# `compile_bind_index_value` (the index-assign RHS compiler) never marked it
# as an escaping position, so it compiled as though it were immediately
# invoked. When the closure's own captured lexical is ALSO assigned anywhere
# in its defining scope -- even inside a branch that never runs -- the
# mutation analysis correctly (if conservatively) flags it as
# captured-and-mutated, but the missing escape flag meant it was never
# boxed into a shared cell. Its capture then fell back to a plain-value
# snapshot, so a same-named PARAMETER of whatever unrelated method the
# closure is eventually invoked from won the lookup instead of the
# closure's own lexical (#8663).
#
# Both classes below are needed: `Inner.invoke` supplies the colliding
# same-named `:$path!` parameter that the closure must not resolve to, and
# the dead `if False` branch is what makes `$path` look mutated to the
# compile-time analysis without ever actually changing its value.
plan 2;

class Inner {
    method invoke(Str :$path!, :%options!) {
        my $func = %options<call>;
        $func(self);
    }
}

class Outer {
    method resource(Str $member) {
        my Str $path = $member;
        $path = 'never' if False;
        my %opts;
        my $seen;
        %opts<call> = sub ($self) {
            $seen = $path;
        };
        Inner.new.invoke(path => '', options => %opts);
        $seen;
    }
}

is Outer.new.resource('hello'), 'hello',
    'closure over a hash-element callback keeps its own captured lexical';

{
    my $path = 99;
    ok $path == 99, 'unrelated same-named sibling lexical';
}

done-testing;
