use Test;

# The late-row walk reads the roles a built-in class composes, so
# `Blob.unpack` and `Enumeration.enums` are rows reached through a
# `Buf[uint8]`'s and an enum's own type (ADR-11276 §9.143). Answers checked
# against Rakudo.

use experimental :pack;

plan 8;

enum Color <red green blue>;
is-deeply Color.enums, Map.new((red => 0, green => 1, blue => 2)), 'enums of an enum type';
is-deeply red.enums, Map.new((red => 0, green => 1, blue => 2)), 'enums of an enum value';
is-deeply Bool.enums, Map.new((False => 0, True => 1)), 'enums of Bool';
is-deeply True.enums, Map.new((False => 0, True => 1)), 'enums of a Bool value';
dies-ok { Int.enums }, 'a non-enum has no enums';

is-deeply Buf.new(1, 2, 3, 4).unpack("C*"), (1, 2, 3, 4), 'Buf.unpack';
is buf8.new(65, 66).unpack("A*"), 'AB', 'buf8.unpack';
is-deeply Blob.new(7).unpack("C"), 7, 'Blob.unpack';
