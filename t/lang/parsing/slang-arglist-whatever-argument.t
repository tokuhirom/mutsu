use lib 't/lib';
use Test;

# A slang reading arguments with the core `<arglist>` (#16888, EEL 0.3's index
# descriptors) sees a `*` that a priming curries as a
# `RakuAST::WhateverCode::Argument`, as the whole unit's parse classifies it,
# and a bare `*` argument as a `RakuAST::Term::Whatever`; its operators resolve
# to the setting's declarations, and a plain string has its compile-time value.

plan 5;

use ArglistKindsSlang;

is %%kinds(:a(*.starts-with('x')), :b(* > 3), :c(*), :d(1)),
    'postfix:RakuAST::WhateverCode::Argument infix:RakuAST::WhateverCode::Argument RakuAST::Term::Whatever RakuAST::IntLiteral',
    'priming stars are arguments, a bare star is a term';

is %%resolved(:a(* > 3), :b(1 <= *), :c(* == 2)),
    '&infix:«>»/True/True &infix:«<=»/True/True &infix:<==>/True/True',
    'an operator resolves to the setting, spelled as rakudo spells it';
is %%resolved(:p(*.starts-with('/api/'))), '/api/',
    "a plain string argument's maybe-compile-time-value";

is %%resolved(:a(any(1, 5))), 'setting', 'a core routine call resolves to the setting';
{
    sub any(*@a) { 'mine' }
    is %%resolved(:a(any(1, 5))), 'not-setting',
        'a routine the enclosing block declares shadows the setting';
}
