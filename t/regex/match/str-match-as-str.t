use Test;

# `Str.match(Regex, :as(Str))` answers with the matched text rather than the
# Match, in every form but `:c`/`:p` (rakudo's `:$as!` candidate; roast
# S05-metasyntax/angle-brackets.t, #18740).

plan 9;

is-deeply 'abc'.match(/a<(bc)>/, :as(Str)), 'bc', ':as(Str) gives the text, <( )> applied';
is-deeply 'abc'.match(/x/, :as(Str)), Nil, 'a miss stays Nil';
is-deeply 'a1b2c3'.match(/\d/, :g, :as(Str)), ('1', '2', '3'), ':g gives a list of texts';
is-deeply 'a1b2c3'.match(/\d/, :as(Str), :x(2)), ('1', '2'), ':x';
is-deeply 'a1b2c3'.match(/\d/, :as(Str), :nth(2)), '2', ':nth';
is-deeply 'a1b2c3'.match(/\d/, :as(Str), :2nd), '2', ':2nd';
isa-ok 'a1b2c3'.match(/\d/, :as(Str), :p(1)), Match, ':p ignores :as';
isa-ok 'a1b2c3'.match(/\d/, :as(Str), :c(0)), Match, ':c ignores :as';
isa-ok 'abc'.match(/b/, :as(Int)), Match, 'only Str is taken';
