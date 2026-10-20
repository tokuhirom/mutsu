use Test;
use MONKEY-SEE-NO-EVAL;
use lib 't/lib';
plan 14;

{
    use PrivateDeclarationConstants;
    is (class { method value { 11 } }).new.value, 11,
        'private package constants do not hide anonymous class syntax';
    is (role { method value { 12 } }).new.value, 12,
        'private package constants do not hide anonymous role syntax';
    is (grammar { method value { 13 } }).new.value, 13,
        'private package constants do not hide anonymous grammar syntax';
}

{
    use ImportedDeclarationConstants;
    is (class { method value { 21 } }).new.value, 21,
        'anonymous class syntax precedes a same-named imported value';
    is (role { method value { 22 } }).new.value, 22,
        'anonymous role syntax precedes a same-named imported value';
    is (grammar { method value { 23 } }).new.value, 23,
        'anonymous grammar syntax precedes a same-named imported value';
    is (class NamedClass { method value { 31 } }).new.value, 31,
        'named class expressions retain declaration syntax';
    is (role NamedRole { method value { 32 } }).new.value, 32,
        'named role expressions retain declaration syntax';
    is (grammar NamedGrammar { method value { 33 } }).new.value, 33,
        'named grammar expressions retain declaration syntax';
    is e + 1, 43, 'an imported value still shadows a core constant';
}

ok abs(e - 2.718281828459045) < 1e-12,
    'a block-local value import does not hide the outer setting constant';
{
    use ReexportDeclarationConstant;
    is e, 42, 'an export hook can re-export a value imported inside a module block';
}
ok abs(e - 2.718281828459045) < 1e-12,
    'a re-exported value remains local to the importing block';

dies-ok {
    EVAL('sub term:<role>() { 42 }; my $r = role { method value { 7 } }; $r.new.value')
}, 'an explicit categorical term keeps precedence over declaration syntax';
