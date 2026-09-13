use v6;
use lib 't/lib';
use Test;
use EvalImportedConstantClass;

# Math::Constants uses EVAL to resolve exported constants named by strings.
# An imported sigilless constant must remain visible to that nested parse.
use MONKEY-SEE-NO-EVAL;

plan 2;

is EVAL_IMPORTED_CLASS_CONST, 42,
    'an exported `my constant` is visible in the importing unit';
is EVAL('EVAL_IMPORTED_CLASS_CONST'), 42,
    'an exported `my constant` is visible inside EVAL';
