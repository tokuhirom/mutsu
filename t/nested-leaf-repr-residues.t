use v6;
use Test;

plan 8;

# Nested-leaf repr residues found by the 2026-07-22 repr oracle sweep: the
# pure container renderer lacked arms for these leaves, so they fell through
# to their bare string values inside containers (the bare reprs were right).

enum Color <red green>;
is red.raku, 'Color::red', 'bare enum .raku (pinned)';
my @a = red, green;
is @a.raku, '[Color::red, Color::green]', 'enums nested in an array stay qualified';
is (:c(red),).hash.raku, '{:c(Color::red)}', 'enum nested in a hash stays qualified';

is ("a".NFC).raku, 'Uni.new(0x0061).NFC', 'bare Uni .raku (pinned)';
is ["a".NFC].raku, '[Uni.new(0x0061).NFC]', 'Uni nested in an array keeps its constructor form';

is [v1.2.3].raku, '[v1.2.3]', 'Version nested .raku (pinned)';
is [v1.2.3].gist, '[v1.2.3]', 'Version nested in an array gist keeps the v prefix';
is [Order::Less].raku, '[Order::Less]', 'built-in enum nested in an array stays qualified';
