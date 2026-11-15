use Test;
use lib 't/lib';

plan 3;

use HookTypeDirect;
use HookTypeMethod;

sub direct(HookDirectType $value) { $value.^name }
sub through-method(HookMethodType $value) { $value.^name }

is direct(HookDirectType), 'HookTypeDirectBase::HookDirectType',
    'a direct stash re-export supplies a parameter type';
is through-method(HookMethodType), 'HookTypeMethodBase::HookMethodType',
    'a stash returned through a method supplies a parameter type';
throws-like { EVAL q[use HookTypeDirect; sub f(HookHiddenType $value) { 1 }] },
    X::Parameter::InvalidType,
    'a class absent from the referenced stash remains invalid',
    typename => 'HookHiddenType';
