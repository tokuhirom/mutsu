use v6;
use Test;

plan 2;

is 'hello'.trans(/l/ => { .uc }), 'heLLo',
    'trans replacement reads its match as the topic';
is 'hello'.trans(/l/ => { @_[0].Str.uc }), 'heLLo',
    'trans replacement receives each match as an argument';
