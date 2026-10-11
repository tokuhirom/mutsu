use Test;

plan 4;

ok (so "a1" ~~ / :my token T { \d }; a <T> /), ':my token is usable inside its own regex';
ok (so "5" ~~ / <T> /), ':my token stays visible to a later regex in the same scope';

{
    "a1" ~~ / :my token V { \d }; a <V> /;
    ok (so "5" ~~ / <V> /), 'visible to a later regex in the same block';
}
dies-ok { "5" ~~ / <V> / }, 'not visible once the declaring block has exited';
