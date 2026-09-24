use Test;

plan 2;

grammar OptionalSeparatorWhitespace {
    proto rule combinator {*}
    rule combinator:sym<plus> { '+' }
    rule word { <.alpha>+ }
    rule TOP { <word> +% <op=.combinator>? }
}

ok OptionalSeparatorWhitespace.subparse('A IMG'),
    'a rule separator can consume implicit whitespace when its operator is absent';
ok OptionalSeparatorWhitespace.subparse('A+IMG'),
    'the same separator still consumes its explicit operator';
