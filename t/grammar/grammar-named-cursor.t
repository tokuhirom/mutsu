use Test;

# `Cursor` is a core alias of `Match`, but a user type of that name is its
# own type, not `Match` (#11705).

plan 9;

{
    # Undeclared, the name still aliases Match.
    is-deeply Cursor, Match, 'bare Cursor is Match';
    ok ("a" ~~ /a/) ~~ Cursor, 'a Match smartmatches Cursor';
    grammar G { has $.x; token TOP { a } }
    is G.parse("a").Str, "a", 'a grammar still parses';
}

{
    my grammar Cursor { token TOP { a } }
    is Cursor.parse("a").Str, "a", 'grammar Cursor parses with its own TOP';
    is Cursor.^name, "Cursor", 'grammar Cursor keeps its name';
}

{
    my class Cursor { method hi { "hi" } }
    is Cursor.hi, "hi", 'class Cursor dispatches its own method';
    ok Cursor.new ~~ Cursor, 'an instance smartmatches its class';
    nok Cursor.new ~~ Match, 'an instance is not a Match';
    sub f(Cursor $c) { $c.hi }
    is f(Cursor.new), "hi", 'a Cursor-typed parameter binds the user class';
}
