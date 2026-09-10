use Test;

# `enum E (Foo, Bar)` with *bare identifiers* in the parenthesised body is an
# undeclared-symbol error (only the `<...>` word-list form autoquotes keys).
# Pairs (`Foo => 1`) and the `<...>` form stay valid.

plan 7;

throws-like 'enum Animal (Cat, Dog)', X::Undeclared::Symbols,
    'enum with bare paren terms is X::Undeclared::Symbols';
throws-like 'enum Color (red, green, blue)', X::Undeclared::Symbols,
    'enum with several bare paren terms';

# The autoquoting word-list form is fine.
lives-ok { EVAL 'enum E1 <Cat Dog>' }, 'enum <...> word list lives';

# Pair entries (bare LHS is an autoquoted key) are fine.
lives-ok { EVAL 'enum E2 (A => 1, B => 2)' }, 'enum (A => 1, ...) pairs live';

# Runtime sanity: the valid forms produce working enums.
my @r = do {
    enum Suit <Hearts Spades>;
    enum Rank (Low => 1, High => 13);
    (Hearts.value, Spades.value, Low.value, High.value)
};
is @r[0], 0, 'word-list enum value 0';
is @r[1], 1, 'word-list enum value 1';
is @r[3], 13, 'pair enum value';
