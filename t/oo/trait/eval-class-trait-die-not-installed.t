use Test;

# A class declaration in an EVAL whose method trait dies while the unit
# compiles is discarded with the unit (rakudo), so a later EVAL can declare
# the same name. A class that compiled and then died at run time stays.

plan 5;

multi sub trait_mod:<is>(Method $m, :$boom!) { die "trait died" }
multi sub trait_mod:<is>(Method $m, :$other!) { 1 }

try { EVAL 'class Baz { method a is boom { 1 } }' };
ok $!.so, 'the trait died inside the first EVAL';
lives-ok { EVAL 'class Baz { method b { 2 } }' }, 'the name can be declared again';
is ::("Baz").new.b, 2, 'and the second declaration is the installed one';

try { EVAL 'class Kept { }; die "run time"' };
throws-like { EVAL 'class Kept { }' }, X::Redeclaration,
    'a class that compiled before a run-time death stays declared';

lives-ok { EVAL 'class Other { method a is other { 1 } }' }, 'a class whose trait lives is unaffected';
