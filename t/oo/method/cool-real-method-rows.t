use Test;

# ADR-11276 slice 3B: Cool's numeric methods (abs, sign, floor, ceiling,
# truncate, round and round($scale)) and Bool's succ and pred are handler rows.
# Rakudo's Cool bodies are `self.Numeric.METHOD`, so a Str parses, a List or
# Hash is its element count, and a non-numeric Str is the X::Str::Numeric
# Failure. Expected values are Rakudo's.

plan 9;

subtest 'a Str numifies', {
    plan 8;
    is "5.5".abs, 5.5, 'abs of a decimal string';
    is-deeply "-5.9".abs, 5.9, 'abs keeps the Rat a decimal numifies to';
    is "6+8i".abs, 10, 'abs of a Complex string';
    is "-3".sign, -1, 'sign';
    is "2.5".round, 3, 'round';
    is "2.5".floor, 2, 'floor';
    is "2.5".ceiling, 3, 'ceiling';
    is "-2.5".truncate, -2, 'truncate';
}

subtest 'a List, an Array and a Hash are their element count', {
    plan 6;
    is [1, 2, 3].floor, 3, 'Array.floor';
    is (1, 2, 3, 4).ceiling, 4, 'List.ceiling';
    is %(a => 1, b => 2).round, 2, 'Hash.round';
    is [1, 2].abs, 2, 'Array.abs';
    is %(a => 1).sign, 1, 'Hash.sign';
    is (1, 2, 3).truncate, 3, 'List.truncate';
}

subtest 'a non-numeric Str is the X::Str::Numeric failure', {
    plan 4;
    throws-like { "abc".abs }, X::Str::Numeric, 'abs';
    throws-like { "abc".floor }, X::Str::Numeric, 'floor';
    throws-like { "abc".round }, X::Str::Numeric, 'round';
    throws-like { "abc".round(2) }, X::Str::Numeric, 'round with a scale';
}

subtest 'round($scale) takes the scale\'s type', {
    plan 9;
    is 1234.round(100), 1200, 'an Int scale';
    is-deeply 1234.round(100), 1200, 'is an Int';
    is-deeply 2.37.round(0.1), 2.4, 'a Rat scale gives a Rat';
    is-deeply 7.round(2), 8, 'half rounds up';
    is-deeply 7.5e0.round(0.5e0), 7.5e0, 'a Num scale gives a Num';
    is-deeply 1000.round(23.01), 989.43, 'a Rat scale stays exact';
    is-deeply <1.5>.round(0.5), 1.5, 'an allomorph receiver';
    is-deeply 7.round(<2>), 8, 'an allomorph scale';
    is-deeply -39.round(0.1), -39.0, 'no float noise in the product';
}

subtest 'round($scale) numifies a Cool receiver and a Str scale', {
    plan 5;
    is "7".round(2), 8, 'a Str receiver';
    is 7.round("2"), 8, 'a Str scale';
    is [1, 2, 3].round(2), 4, 'an Array receiver';
    is-deeply <2.5+3.5i>.round(1), <3+4i>, 'a Complex receiver rounds each part';
    is-deeply 5.round(1), 5, 'an Int scale of one';
}

subtest 'Bool is an Int enum', {
    plan 6;
    is-deeply True.floor, True, 'True.floor is True itself';
    is-deeply False.ceiling, False, 'False.ceiling';
    is True.abs, 1, 'True.abs';
    is False.sign, 0, 'False.sign';
    is-deeply False.succ, True, 'False.succ';
    is-deeply True.pred, False, 'True.pred';
}

subtest 'Bool.succ and Bool.pred saturate', {
    plan 4;
    is-deeply True.succ, True, 'True.succ is True';
    is-deeply False.pred, False, 'False.pred is False';
    is-deeply Bool.succ.^name, 'Bool', 'Bool declares succ';
    ok Bool.^can('succ') && Bool.^can('pred'), 'the Bool method table has both';
}

subtest 'type objects keep the concreteness check', {
    plan 2;
    throws-like { Int.abs }, X::Parameter::InvalidConcreteness, 'Int.abs';
    throws-like { Int.floor }, X::Parameter::InvalidConcreteness, 'Int.floor';
}

subtest 'the method table exposes the rows', {
    plan 3;
    ok Cool.^can('abs') && Cool.^can('sign') && Cool.^can('floor') && Cool.^can('ceiling')
        && Cool.^can('truncate') && Cool.^can('round'), 'Cool declares them all';
    is-deeply (^3).map({ "2.5".round }).List, (3, 3, 3), 'a repeated call site answers each time';
    is-deeply (^3).map({ [1, 2].floor }).List, (2, 2, 2), 'for an Array too';
}
