use Test;

# Slice F (env<->locals coherence): `import` writes the imported symbols into
# `env` by name; a later bare reference (e.g. an imported `constant c`) must be
# written through to the caller's local slot so it is coherent even with the
# reverse env->locals pull disabled (MUTSU_NO_REVERSE_SYNC=1). The ImportModule
# opcode drains the names import_module recorded.

plan 6;

{
    module A {
        sub a() is export { 41 }
        constant c is export = 10;
    }
    import A;
    is a(), 41, 'imported sub is callable';
    is c, 10, 'imported constant is visible to the caller';
}

{
    module B {
        constant PI is export = 3;
        constant E is export = 2;
    }
    import B;
    is PI, 3, 'first imported constant visible';
    is E, 2, 'second imported constant visible';
}

{
    module D {
        constant greeting is export = 'hi';
    }
    import D;
    # Use the imported constant in an expression to exercise the local slot read.
    is greeting ~ '!', 'hi!', 'imported constant usable in an expression';
}

{
    module F {
        constant SIX is export = 6;
    }
    import F;
    my $doubled = SIX * 2;
    is $doubled, 12, 'imported constant participates in arithmetic';
}
