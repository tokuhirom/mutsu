use Test;

# A trailing comma is legal in every Raku list literal, including the one that
# initializes an attribute. mutsu used to hand-roll the `has @.a = 1, 2` comma
# split without the shared comma-list rules, so a trailing comma made the whole
# `has` declaration fail to parse (falling back to `BareWord("has")` plus a
# plain assignment) -- which silently dropped the initializer AND, for a
# `%`/`@`-sigil attribute, the generated accessor along with it.

plan 16;

class A { has @.a = 1, 2,; has $.b = 3; method m { @!a } }
is A.new.m.raku, '[1, 2]', 'trailing comma in @-attribute default keeps the list';
is A.new.a.raku, '[1, 2]', 'the accessor is still generated';
is A.new.b, 3, 'an adjacent attribute is unaffected';

class B { has %.t = a => 1, b => 2,; }
is B.new.t.raku, '{:a(1), :b(2)}', 'trailing comma in %-attribute default';

class C { has $.x = 5,; }
is C.new.x.raku, '5', 'trailing comma after a $-attribute default';

class E { has Int @.e = 1, 2,; has Str $.s = 'z'; }
is E.new.e.raku, 'Array[Int].new(1, 2)', 'trailing comma with an element type';
is E.new.s.raku, '"z"', 'adjacent typed scalar attribute is unaffected';

class G { has @.g = 1, 2,; has @.h = 3, 4,; }
is G.new.g.raku, '[1, 2]', 'two attributes with trailing commas (first)';
is G.new.h.raku, '[3, 4]', 'two attributes with trailing commas (second)';

# A single element plus a trailing comma is a one-slot list, and that slot
# stays unflattened -- exactly as `my @a = 1..5,` behaves.
class K { has @.k = 1,; }
is K.new.k.raku, '[1]', 'single element with a trailing comma';
class R { has @.r = 1..5,; }
is R.new.r.raku, '[1..5,]', 'a trailing comma keeps a Range unflattened';
class S { has @.s = (1, 2),; }
is S.new.s.raku, '[(1, 2),]', 'a trailing comma keeps a parenthesized list unflattened';

# The same splitter now skips whitespace before the comma, so a list may be
# broken across lines.
class H { has @.a = 1 , 2; }
is H.new.a.raku, '[1, 2]', 'whitespace before the comma';
class X {
    has @.x =
        1,
        2,
    ;
}
is X.new.x.raku, '[1, 2]', 'multi-line list with a trailing comma';

# Private attributes and role-composed attributes go through the same parse.
class T { has @!p = 1, 2,; method get { @!p } }
is T.new.get.raku, '[1, 2]', 'private @-attribute with a trailing comma';

role Ro { has @.q = 7, 8,; }
class U does Ro { }
is U.new.q.raku, '[7, 8]', 'role attribute with a trailing comma';
