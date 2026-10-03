use Test;

# X::Parameter::WrongOrder names the misplaced parameter as it is spelled
# (#11373): no extra `$` on an array/hash/code parameter, a sigilless one is
# bare, an anonymous one is its sigil. Expected values are Rakudo's.

my @cases =
    'sub g(:$a, @p) { }' => ('@p', 'required', 'named'),
    'sub g(*@a, &f) { }' => ('&f', 'required', 'variadic'),
    'sub g($a?, %h) { }' => ('%h', 'required', 'optional'),
    'sub g(:$a, \x) { }' => ('x',  'required', 'named'),
    'sub g(:$a, $) { }'  => ('$',  'required', 'named'),
    'sub g(:$a, @) { }'  => ('@',  'required', 'named'),
    'sub g(:$a, $b) { }' => ('$b', 'required', 'named');

plan 2 * @cases;

for @cases -> (:key($code), :value(($param, $misplaced, $after))) {
    try EVAL $code;
    isa-ok $!, X::Parameter::WrongOrder, "$code: X::Parameter::WrongOrder";
    is $!.message, "Cannot put $misplaced parameter $param after $after parameters",
        "$code: message spells $param";
}
