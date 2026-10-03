use v6;
use Test;

plan 7;

# A `%` / `%%` separator on a quantified token that is preceded by other
# tokens repeats only that token (TOML::Thumb's `'0b' [ <[0..1]>+ ]+ % _`).

is ("b1-0" ~~ / b [ \d ]+ % "-" /).Str, 'b1-0', 'literal prefix, group atom';
is ("b1-0" ~~ / b[\d]+ % "-" /).Str, 'b1-0', 'no whitespace';
is ("b1-0" ~~ / . [ \d ]+ % "-" /).Str, 'b1-0', 'any-char prefix';
is ("b1-0" ~~ / b [ \d ]+ %% "-" /).Str, 'b1-0', '%% form';
is ("b1-0-" ~~ / b [ \d ]+ %% "-" /).Str, 'b1-0-', '%% takes a trailing separator';
is ("0b1_0_1" ~~ / | 0 | '0b' [ <[0..1]>+ ]+ % _ /).Str, '0b1_0_1', 'inside an LTM alternation';
is ("xa,b" ~~ / x <[ab]>+ % "," /).Str, 'xa,b', 'char-class atom after a prefix';
