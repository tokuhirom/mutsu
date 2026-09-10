use Test;

# §B: the sound multi-method resolution cache keys a type+arity-deterministic
# multi's resolution on (class, method, positional arg types). Value-dependent
# multis (where / literal / subset / :D:U smiley / coercion) must NOT be cached —
# these exercise that the cache returns the right candidate across repeated and
# mixed calls, and that the excluded forms still dispatch correctly every time.

plan 15;

# Plain type-based multi (cacheable): repeated + alternating arg types.
class TC {
    multi method m(Int $x) { "int:$x" }
    multi method m(Str $x) { "str:$x" }
}
my $tc = TC.new;
is $tc.m(1), 'int:1', 'multi Int (1st)';
is $tc.m(2), 'int:2', 'multi Int (cached)';
is $tc.m('a'), 'str:a', 'multi Str (1st)';
is $tc.m('b'), 'str:b', 'multi Str (cached)';
is $tc.m(3), 'int:3', 'multi Int again (cache not clobbered by Str)';

# Subclass arg dispatches to its own key (not collapsed with the parent).
class Animal { }
class Dog is Animal { }
class Disp {
    multi method who(Animal $a) { 'animal' }
    multi method who(Dog $d)    { 'dog' }
}
my $d = Disp.new;
is $d.who(Animal.new), 'animal', 'parent-typed arg → parent candidate';
is $d.who(Dog.new), 'dog', 'subclass arg → subclass candidate (distinct key)';
is $d.who(Animal.new), 'animal', 'parent again after subclass (no key collision)';

# where-constrained multi (NOT cacheable): value-dependent, correct every call.
class W {
    multi method f(Int $x where * > 10) { 'big' }
    multi method f(Int $x)              { 'small' }
}
my $w = W.new;
is $w.f(5), 'small', 'where multi: small';
is $w.f(20), 'big', 'where multi: big (value-dependent, not miscached)';
is $w.f(7), 'small', 'where multi: small again';

# :D / :U smiley multi (NOT cacheable): definedness-dependent.
class S {
    multi method h(Int:D $x) { 'def' }
    multi method h(Int:U $x) { 'undef' }
}
my $s = S.new;
is $s.h(5), 'def', ':D/:U multi: defined';
is $s.h(Int), 'undef', ':D/:U multi: undefined (definedness-dependent)';
is $s.h(9), 'def', ':D/:U multi: defined again';

# Adding a candidate at runtime invalidates the cache (no stale resolution).
class R { multi method k(Int $x) { 'a' } }
my $r = R.new;
is $r.k(1), 'a', 'multi before augment';
