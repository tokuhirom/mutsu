use Test;

plan 2;

is (|<a b>, { .succ } ... *)[^7].join(', '), 'a, b, c, d, e, f, g',
    'prefix slip works with < > quote-word lists';

is (|«a b», { .succ } ... *)[^7].join(', '), 'a, b, c, d, e, f, g',
    'prefix slip works with « » quote-word lists';
