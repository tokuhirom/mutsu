use Test;

# `&!attr` names the invocant's own private `&`-sigil attribute. A method
# called from another object's method used to resolve `&!attr` to the CALLER's
# same-named `$!attr`, which sits in the env under the same sigil-less `!attr`
# key: calling it died with "No such method 'CALL-ME' for invocant of type
# 'Int'". CSS::Module's `method index { &!index() }`, reached from
# CSS::Properties (which has its own `$!index`), was the case found in the wild
# (#10662). Expected values are rakudo's.

plan 9;

class Callee {
    has &!v = sub { 'callee' };
    method call { &!v() }
    method call-dot { &!v.() }
    method what { &!v.WHAT.raku }
    method defined { &!v.defined }
}

class Caller {
    has $!v = 2;
    has $.callee;
    method via-self { $!v = 3; self.go }
    method go { ($!callee.call, $!callee.call-dot, $!callee.what, $!callee.defined) }
    method direct { $!v = 4; $!callee.call }
}

{
    my @r = Caller.new(:callee(Callee.new)).via-self;
    is @r[0], 'callee', '`&!v()` calls the invocant\'s attribute, not the caller\'s `$!v`';
    is @r[1], 'callee', '`&!v.()` too';
    is @r[2], 'Sub', 'a plain `&!v` read sees the invocant\'s Sub';
    ok @r[3], '`&!v.defined`';
    is Caller.new(:callee(Callee.new)).direct, 'callee', 'called straight from the caller\'s method';
}

# The reduced CSS::Module / CSS::Properties shape: a `has &.index` read
# through `method index { &!index() }` from a class with its own `$!index`.
{
    module Meta { our sub index { state $ = [1, 2, 3] } }
    class Mod { has &.index; method index { &!index() } }
    class Props {
        has $.module;
        has $!index;
        has $.seen;
        submethod TWEAK { $!index = $!module.index; self.go }
        method go { $!seen = $!module.index[1] }
    }
    is Props.new(:module(Mod.new(:index(&Meta::index)))).seen, 2,
        'an `&.index` attribute read from a class with its own `$!index`';
}

# The reverse direction: the caller has the `&` attribute, the callee the `$`.
{
    class ScalarCallee { has $!v = 1; method get { $!v } }
    class CodeCaller {
        has &!v = sub { 9 };
        has $.s;
        method run { ($!s.get, &!v()) }
    }
    my @r = CodeCaller.new(:s(ScalarCallee.new)).run;
    is @r[0], 1, 'the callee\'s `$!v`';
    is @r[1], 9, 'the caller\'s own `&!v`';
}

# A role's private `&` attribute in a class with a same-named `$` attribute.
{
    role HasHook { has &!h = { 'role' }; method r { &!h() } }
    class Hooked does HasHook { method m { self.r } }
    is Hooked.new.m, 'role', 'a role\'s `&!h()`';
}
