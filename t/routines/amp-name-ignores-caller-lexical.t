use v6;
use Test;

# `&name` inside a routine names the routine visible where that routine was
# written, not a same-named `my &name` of whoever called it. A caller's
# `my &tab-up = -> |c { $obj.tab-up(|c) }` made `method tab-up(|c) { &tab-up(|c) }`
# call itself until the stack ran out (Template::HAML's direct-emit helpers,
# #10638). A bare `name()` call already got this right.

plan 9;

sub g() { 'sub' }

sub call-amp() { &g() }
sub read-amp() { &g.name }
sub call-bare() { g() }

{
    my &g = sub lexical() { 'lexical' };
    is call-amp(), 'sub', '&g() in a routine ignores the caller lexical';
    is read-amp(), 'g', '&g in a routine ignores the caller lexical';
    is call-bare(), 'sub', 'bare g() (already right)';
    is &g(), 'lexical', 'the lexical is still seen where it is in scope';
    is (-> { &g() })(), 'lexical', 'and from a closure that captured it';
}

class C {
    method g() { &g() }
}
{
    my $c = C.new;
    my &g = -> { $c.g };
    is g(), 'sub', 'method delegating to the same-named sub does not recurse';
}

sub with-param(&g) { &g() }
is with-param(sub param() { 'param' }), 'param', 'a &g parameter wins';

sub with-named(:&g) { &g() }
is with-named(g => sub named() { 'named' }), 'named', 'a :&g parameter wins';

{
    my &g = sub lexical() { 'lexical' };
    sub inner() { &g() }
    is inner(), 'lexical', 'a routine declared inside the lexical scope sees it';
}
