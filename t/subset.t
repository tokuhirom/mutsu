use Test;
plan 3;

subset PositiveInt of Int where $_ > 0;

sub grab(PositiveInt $x) { return $x }

is grab(1), 1, 'subset accepts positive int';
dies-ok { grab(0) }, 'subset rejects zero';
dies-ok { grab(-1) }, 'subset rejects negative';
