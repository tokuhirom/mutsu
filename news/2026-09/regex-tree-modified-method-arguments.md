# Regex RakuAST lowering keeps method dispatch modifiers

RakuAST regex trees with an argumented subrule can now lower a dispatch-modified
method-call argument such as `<word($value.?uc)>`. The existing match-time
regex argument evaluator receives the original `.?`, `.+`, or `.*` spelling,
so RakuAST EVAL and direct grammar matching keep dynamic lexical behavior.

Closes #8033.
