use Test;

# `.raku` of an enum value leaves the name bare only when every character is
# a word character; anything else (a hyphen, quote, space, dot, `:`) is
# quoted as `Enum::<name>` (#16992).
plan 12;

enum Q (:A(0), 'At-least-once' => 1, 'a b' => 2, 'x:y' => 3, '1x' => 4, 'a-1' => 5,
        "it's" => 6, 'é' => 7, 'a--b' => 8, 'a.b' => 9, '_x' => 10, 'x1' => 11);

my %want =
    A => 'Q::A', 'At-least-once' => 'Q::<At-least-once>', 'a b' => 'Q::<a b>',
    'x:y' => 'Q::<x:y>', '1x' => 'Q::1x', 'a-1' => 'Q::<a-1>',
    "it's" => "Q::<it's>", 'é' => 'Q::é', 'a--b' => 'Q::<a--b>',
    'a.b' => 'Q::<a.b>', '_x' => 'Q::_x', 'x1' => 'Q::x1';

for %want.sort(*.key) -> $p {
    is Q::{$p.key}.raku, $p.value, "{$p.key.raku} .raku";
}
