use Test;

plan 6;

is samecase('Raku', 'abcdE'), 'raku', 'samecase as function';
is ''.samecase(''), '', 'samecase empty string';
is 'Hello World !'.samecase('AbCdEfGhIjKlMnOpQrStUvWxYz'), 'HeLlO WoRlD !', 'samecase literal';
is 'hello'.samecase('A'), 'HELLO', 'samecase extends last pattern char';
is 'HELLO'.samecase('ab'), 'hello', 'samecase extends lowercase';
is 'abc'.samecase('XxX'), 'AbC', 'samecase alternating pattern';
