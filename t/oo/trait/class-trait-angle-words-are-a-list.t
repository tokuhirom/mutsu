use v6;
use Test;

# From Red (`model Bla is handling<map grep> { ... }`): a custom `is` trait on a
# class or role written with an angle-bracket word list receives the words as a
# list, like `<map grep>` anywhere else, not one "map grep" string.

plan 5;

our @seen;
multi trait_mod:<is>(Mu:U $type, :$words) {
    @seen.push: $words.raku;
    for $words<> -> $w { $type.^add_method: $w, method (|c) { "custom $w" } }
}

class Two is words<grep first> { has $.a = 1 }
is @seen[0], '$("grep", "first")', 'two words arrive as a list';
is Two.grep(*.a), 'custom grep', 'the first word names an added method';
is Two.first(*.a), 'custom first', 'and the second';

class One is words<zork> { }
is One.zork, 'custom zork', 'a single word still works';
is @seen[1], '"zork"', 'and stays a plain string';
