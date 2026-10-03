use v6;
use Test;

plan 5;

# Inside an embedded `{ ... }` block, `$/` honours a `<(` / `)>` already
# passed, as the final match does (TOML::Thumb's multi-line literal string:
# `"'''" \n? <( ... )> "'''" { make ~$/ }`).

is ("xab" ~~ / x <( a )> b { make ~$/ } /).made, 'a', '<( and )> both passed';
is ("xab" ~~ / x <( a b { make ~$/ } /).made, 'ab', 'only <( passed';
is ("xab" ~~ / x a )> b { make ~$/ } /).made, 'xa', 'only )> passed';
is ("a'q'" ~~ / a [ "'" <( .*? )> "'" { make ~$/ } ] /).made, 'q', 'inside a group';
is ("zxab" ~~ / x <( a b { make $/.from ~ "-" ~ $/.to } /).made, '2-4', '.from and .to';
