use Test;

# A typed `my` of a user class or role, executed repeatedly from one site:
# each execution must resolve the constraint as the first did, and pick up
# whatever renames it since (#12917's per-site declaration memo).

plan 21;

role Tag { }
class ShortTag does Tag { }
class Other { }

class C {
    method m($x) { my Tag $t = $x; $t }
    method cls($x) { my Other $o = $x; $o }
}

my $c = C.new;
is $c.m(Nil).^name, 'Tag', 'a Nil leaves the role type object (first call)';
is $c.m(Nil).^name, 'Tag', 'a Nil leaves the role type object (memoized call)';
is $c.m(ShortTag.new).^name, 'ShortTag', 'a value doing the role is kept';
throws-like { $c.m(Other.new) }, X::TypeCheck::Assignment,
    'a value of another type is refused on a memoized site';
throws-like { $c.m(Other.new) }, X::TypeCheck::Assignment,
    'and refused again';
is $c.cls(Nil).^name, 'Other', 'a class constraint seeds its type object';
is $c.cls(Other.new).^name, 'Other', 'a class constraint keeps its instance';

# The same site run in a loop, and as the expression-position form.
my @seen = (^5).map({ $c.m(Nil).^name });
is @seen.join(','), 'Tag,Tag,Tag,Tag,Tag', 'every execution seeds the same type object';

# A lexical class shadowing a global one of the same name is what the
# inner declaration means.
class Shadow { method who { 'global' } }
sub outer-shadow() { my Shadow $s; $s.who }
sub inner-shadow() {
    my class Shadow { method who { 'lexical' } }
    my Shadow $s;
    $s.who
}
is outer-shadow(), 'global', 'the global class (first)';
is inner-shadow(), 'lexical', 'the lexical class wins inside its scope';
is outer-shadow(), 'global', 'the global class again';

# A `::T` capture spelled like nothing registered still resolves per call.
sub cap(::T $v) { my T $x; $x.^name }
is cap(1), 'Int', 'a capture resolves to its first binding';
is cap('a'), 'Str', 'and to each later one';

# A site first run after many others still resolves on its own.
sub late() { my Tag $t = Nil; $t.^name }
is late(), 'Tag', 'a late call still seeds the role';

# A type declared inside a package, named by its short spelling.
module Pk {
    role PTag { }
    class PC does PTag { }
    our sub decl($x) { my PTag $t = $x; $t.^name }
}
is Pk::decl(Nil), 'Pk::PTag', 'a package type seeds its qualified type object';
is Pk::decl(Pk::PC.new), 'Pk::PC', 'and keeps a value doing it';

my $msg = (try { $c.m(Other.new) }) // $!.message;
like $msg, /'expected Tag but got Other'/, 'the refusal names both types';

# A Nil initializer reaching a builtin-typed declaration's store.
sub nil-of() { Nil }
sub str-decl() { my Str $s = nil-of(); $s }
is str-decl().^name, 'Str', 'a Nil leaves Str (first call)';
is str-decl().^name, 'Str', 'a Nil leaves Str (memoized call)';
sub default-decl() { my Str $s is default('d') = nil-of(); $s }
is default-decl(), 'd', 'an `is default` value takes over a Nil';
sub reassign() { my Tag $t = ShortTag.new; $t = nil-of(); $t.^name }
is reassign(), 'Tag', 'a later Nil assignment resets to the role type object';
