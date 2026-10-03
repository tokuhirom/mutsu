use v6;
use Test;

plan 6;

# A character class whose first part is a NEGATED property starts from every
# character minus that property; a later `+` part is a union with that
# complement (TOML::Thumb's comment token: `<-:Cc +[\t]>`).

is ("ab\ncd" ~~ / <-:Cc +[\t]>* /).Str, 'ab', '<-:Cc +[\t]> stops at a newline';
is ("a\tb\ncd" ~~ / <-:Cc +[\t]>* /).Str, "a\tb", '... but accepts the tab it adds back';
is ("ab\n" ~~ / <:!Cc +[\n]>* /).Str, "ab\n", '<:!Cc +[\n]> spelling';
is ("a;b" ~~ / <-:C-[;]>* /).Str, 'a', 'a pure subtraction tail still folds';
is ("aB1" ~~ / <-:Lu +[B] -[a]>* /).Str, '', 'a later - part subtracts from the union';
is ("x\ty" ~~ / <-[\t] +[x]>* /).Str, 'x', 'bracket form unchanged';
