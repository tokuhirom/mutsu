use Test;

plan 1;

role R2 {...};
role R1 does R2 {};
role R2 {};
class C does R1 {};

is [C ~~ R1, C ~~ R2], [True, True],
    'a role parent declared before its body is upgraded before class composition';
