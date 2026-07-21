use v6;
use Test;

# A quoted MOP pseudo-method call (`$obj."WHAT"()`) dispatches a user-defined
# method of that name, while the bare `.WHAT` term stays the reflection macro.
# (raku-doc Language/typesystem.rakudoc)
{
    class A {
        method WHAT { "ain't gonna happen" }
    }
    is A.new.WHAT.raku, 'A', 'bare .WHAT is the type-object macro';
    is A.new."WHAT"(), "ain't gonna happen", 'quoted ."WHAT"() calls the user method';
}

# The same holds for the other reflection macros.
{
    class B {
        method HOW { "user-HOW" }
        method WHO { "user-WHO" }
        method WHY { "user-WHY" }
        method WHICH { "user-WHICH" }
        method WHERE { "user-WHERE" }
        method DEFINITE { "user-DEFINITE" }
    }
    is B.new."HOW"(), "user-HOW", 'quoted ."HOW"() calls the user method';
    is B.new."WHO"(), "user-WHO", 'quoted ."WHO"() calls the user method';
    is B.new."WHY"(), "user-WHY", 'quoted ."WHY"() calls the user method';
    is B.new."WHICH"(), "user-WHICH", 'quoted ."WHICH"() calls the user method';
    is B.new."WHERE"(), "user-WHERE", 'quoted ."WHERE"() calls the user method';
    is B.new."DEFINITE"(), "user-DEFINITE", 'quoted ."DEFINITE"() calls the user method';
}

# Bare macros keep working when no user method shadows them.
{
    class C {}
    is C.new.WHAT.raku, 'C', 'bare .WHAT still the macro when unshadowed';
    is C.new.HOW.^name, 'Perl6::Metamodel::ClassHOW', 'bare .HOW still the macro';
    is C.new.WHO.Str, 'C', 'bare .WHO still the macro';
}

# An anonymous enum value has no type name: raku reports "".
{
    my $e = enum <one two three>;
    is one.^name, '', 'anonymous enum value .^name is the empty string';
    is $e.^name, 'Map', 'the anon enum itself is a Map';
}

done-testing;
