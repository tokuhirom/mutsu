use Test;
plan 6;

sub msg(&c) { try c(); $!.message }

is msg({ my %h := (1,2) }),
    "Type check failed in binding; expected Associative but got List. You have to pass an explicitly typed\nhash, not one that just might happen to contain elements of the correct\ntype.",
    'a List bound to % gets the explicitly-typed-hash hint';
is msg({ my %h := [1,2] }),
    "Type check failed in binding; expected Associative but got Array. You have to pass an explicitly\ntyped hash, not one that just might happen to contain elements of the\ncorrect type.",
    'an Array bound to % gets the hint';
is msg({ my %h := (1,2).Seq }),
    'Type check failed in binding; expected Associative but got Seq',
    'a Seq bound to % has no hint';
is msg({ my %h := 5 }),
    'Type check failed in binding; expected Associative but got Int',
    'an Int bound to % is named by its type alone';
is msg({ my Int %z := (1,2).Seq }),
    'Type check failed in binding; expected Associative[Int] but got Seq',
    'a typed hash names the parameterized type';
is msg({ my Int %z := [1,2] }),
    "Type check failed in binding; expected Associative[Int] but got Array. You have to pass an explicitly\ntyped hash, not one that just might happen to contain elements of the\ncorrect type.",
    'a typed hash with an Array gets the hint';
