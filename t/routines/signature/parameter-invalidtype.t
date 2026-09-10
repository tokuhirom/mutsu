use Test;

plan 9;

# A sub parameter whose type names an unknown type raises
# X::Parameter::InvalidType, with a "Did you mean" suggestion when close.

throws-like 'sub yoink(Junctoin $barf) { }', X::Parameter::InvalidType,
    suggestions => 'Junction';

throws-like 'sub f(Nonexistent $x) { }', X::Parameter::InvalidType;

throws-like 'multi sub f(Junctoin $x) { }', X::Parameter::InvalidType,
    suggestions => 'Junction';

# Valid types (builtin, user class/role/subset/enum) are accepted, including
# when the type is declared after the sub in the same compilation unit.
lives-ok { EVAL 'sub f(Int $x) { $x }' }, 'builtin type accepted';
lives-ok { EVAL 'class C { }; sub f(C $x) { }' }, 'user class accepted';
lives-ok { EVAL 'role R { }; sub f(R $x) { }' }, 'role accepted';
lives-ok { EVAL 'subset Sm of Int where * > 0; sub f(Sm $x) { }' },
    'subset accepted';
lives-ok { EVAL 'sub f(Forward $x) { }; class Forward { }' },
    'type declared later in the same unit is accepted';

# A bare term param (e.g. `(Inf)`) smartmatches a value, not a type.
lives-ok { EVAL 'multi foo(Inf) { "inf" }; foo(Inf)' },
    'bare value term parameter is not treated as a type';
