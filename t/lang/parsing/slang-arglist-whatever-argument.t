use lib 't/lib';
use Test;

# A slang reading arguments with the core `<arglist>` (#16888, EEL 0.3's index
# descriptors) sees a `*` that a priming curries as a
# `RakuAST::WhateverCode::Argument`, as the whole unit's parse classifies it,
# and a bare `*` argument as a `RakuAST::Term::Whatever`.

plan 1;

use ArglistKindsSlang;

is %%kinds(:a(*.starts-with('x')), :b(* > 3), :c(*), :d(1)),
    'postfix:RakuAST::WhateverCode::Argument infix:RakuAST::WhateverCode::Argument RakuAST::Term::Whatever RakuAST::IntLiteral',
    'priming stars are arguments, a bare star is a term';
