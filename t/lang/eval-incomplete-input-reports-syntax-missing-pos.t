# A parse failure carries rakudo's `X::Comp.pos` (character offset), which a REPL compares with the
# source length to tell "input ended early" from a real error (Jupyter::Kernel::Sandbox) (#16585).
use Test;
use MONKEY-SEE-NO-EVAL;
plan 6;

for 'for (1..10) {', 'if 1 {' -> $code {
    try { EVAL $code; CATCH { default {
        isa-ok $_, X::Syntax::Missing, "$code: X::Syntax::Missing";
        is .pos, $code.chars, "$code: pos is the end of the input";
    } } }
}

try { EVAL 'for (1..10) { say 1 } }'; CATCH { default {
    ok .pos.defined, 'a stray closing brace also has a pos';
    ok .pos < 'for (1..10) { say 1 } }'.chars + 1, 'within the source';
} } }
