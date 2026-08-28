use Test;

plan 8;

enum Color <Red Green Blue>;

# A type object always smartmatches its own type (`Int ~~ Int` is True);
# an enum's type object is no exception, but the enum-specific smartmatch
# arm only ever considered the LHS being an enum VALUE (`Red ~~ Color`),
# never the LHS being the enum's OWN type object compared to itself. That
# made `Color ~~ Color` False when it should be True -- and, in turn, made
# any `$x ~~ Color` matcher fail whenever `$x` held the type object rather
# than a value, e.g. `X::Enum::NoValue.type` (which IS the type object, not
# a member) matched against `type => Color` in a `throws-like` call.
ok Color ~~ Color, 'an enum type object smartmatches itself';
ok Red ~~ Color, 'an enum value still smartmatches its type';
nok Color ~~ Red, 'an enum type object does not smartmatch a member';
nok Str ~~ Color, 'an unrelated type object does not smartmatch the enum';

# Definedness smileys still apply correctly to the type object.
ok Color ~~ Color:U, 'the type object matches its own :U form';
nok Color ~~ Color:D, 'the type object does not match the :D form';
ok Red ~~ Color:D, 'an enum value matches the :D form';
nok Red ~~ Color:U, 'an enum value does not match the :U form';
