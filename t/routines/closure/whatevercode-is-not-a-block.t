use Test;

plan 9;

# WhateverCode is a Code but not a Block (rakudo MRO: WhateverCode, Code, Any, Mu).
ok (*.so) !~~ Block, "*.so is not a Block";
ok (* + 1) !~~ Block, "* + 1 is not a Block";
ok (* + 1) ~~ Code, "a WhateverCode is a Code";
ok (* + 1) ~~ Callable, "a WhateverCode is Callable";
ok {1} ~~ Block, "a bare block is still a Block";

proto f(|) {*}
multi f(Block:D $) { "block" }
multi f(Callable:D $) { "callable" }
is f(* %% 2), "callable", "WhateverCode skips the Block candidate";
is f({ 1 }), "block", "a bare block takes the Block candidate";
is f(-> $x { 1 }), "block", "a pointy block takes the Block candidate";

sub g(Block:D $) { 1 }
throws-like { g(* %% 2) }, X::TypeCheck::Binding::Parameter, "Block:D rejects a WhateverCode";
