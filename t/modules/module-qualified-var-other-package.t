use Test;
use lib $?FILE.IO.parent(2).add('lib');
use QualifiedVarOtherPackage;
use QualifiedVarOtherPackage :override;

# `$State::tz`, read in a routine whose own parameter is `$tz`, is the
# package variable: an unset one is undefined, never the parameter.

plan 4;

is Z::read-qualified('param').raku, 'Any', 'an unset package variable is not the parameter';
set-tz('first');
is current(), 'first', 'set through the qualified name';
set-tz('second');
clear-tz();
is current(), 'first', 'the backup holds the value before the override';
is Z::read-qualified('param'), 'first', 'a set package variable reads its value';
