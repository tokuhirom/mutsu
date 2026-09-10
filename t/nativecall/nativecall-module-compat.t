use v6;
use Test;

# General improvements surfaced while loading the real NativeCall-based module
# stack (NativeLibs / DBDish::SQLite::Native). Each is an independent Raku
# feature, not a DBDish-specific hack.

plan 7;

# 1. `&trait_mod:<is>` — referencing the `is` trait-modifier routine by its
#    extended categorical name (NativeLibs' EXPORT introspects it). Previously
#    only infix/prefix/postfix/term/circumfix categories parsed; `trait_mod`
#    raised a parse error. The fix is parse-level, so the reference just has to
#    compile and evaluate without throwing.
{
    lives-ok { my $code = &trait_mod:<is>; $code }, '&trait_mod:<is> parses and is referenceable';
}

# 2. Rakudo::Internals.IS-WIN / .IS-MACOS platform predicates (used by NativeLibs
#    to choose a library name). They must return Bool values.
{
    my $win = Rakudo::Internals.IS-WIN;
    isa-ok $win, Bool, 'Rakudo::Internals.IS-WIN returns a Bool';
    my $mac = Rakudo::Internals.IS-MACOS;
    isa-ok $mac, Bool, 'Rakudo::Internals.IS-MACOS returns a Bool';
    # Not both true on any real platform.
    nok ($win && $mac), 'IS-WIN and IS-MACOS are not both true';
}

# 3. `is encoded('utf8')` parameter trait (NativeCall string marshalling). The
#    trait name and its parenthesized argument must parse, and the value must
#    pass through unchanged.
{
    sub take-str(Str $s is encoded('utf8')) { $s.uc }
    is take-str('hello'), 'HELLO', 'is encoded(...) param trait accepted; value passes through';
}

# A return-type'd native-style signature with an encoded param also parses.
{
    sub two(Str $a is encoded('utf8'), Int $n --> Str) { $a x $n }
    is two('ab', 3), 'ababab', 'encoded param alongside other params and return type';
}

# Anonymous (type-only) param with the encoded trait parses too.
{
    sub anon(Str $ is encoded('utf8')) { 'ok' }
    is anon('x'), 'ok', 'encoded trait on a named-but-unused param parses';
}
