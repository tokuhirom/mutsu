use v6;
use lib 't/lib';
use Test;
use ParamTypeProvider;

plan 4;

# A class a `use`d module declares is a valid parameter type in the using unit.
# mutsu's compile-time parameter-type check runs before the mainline (and so
# before `use` has loaded anything), which used to reject `sub f(URI $u)` with
# "Invalid typename 'URI'" for every unqualified module-defined class.

sub takes-provider(ParamTypeProvider $p) { $p.label }
is takes-provider(ParamTypeProvider.new(label => 'ok')), 'ok',
    'an unqualified class from a used module is a valid parameter type';

sub takes-subset(ParamTypeSmall $n) { $n * 2 }
is takes-subset(4), 8, 'a subset from a used module is a valid parameter type';

sub takes-enum(ParamTypeColour $c) { ~$c }
is takes-enum(ParamRed), 'ParamRed', 'an enum from a used module is a valid parameter type';

# A genuine typo is still rejected.
throws-like 'sub yoink(Junctoin $barf) { }', X::Parameter::InvalidType,
    'a mistyped type name is still caught';
