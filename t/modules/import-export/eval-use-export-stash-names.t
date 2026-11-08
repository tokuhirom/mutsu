use Test;
use lib 't/lib';
use MONKEY-SEE-NO-EVAL;

plan 3;

# A module that fills its import set through `package EXPORT::<tag> { our ... }`
# brings in names no source scan can list: an EVAL'd unit that `use`s it must
# not reject them as undeclared at CHECK time.
is EVAL(Q[use ExportStashOurVar; $answer]), 42, 'an our scalar of EXPORT::DEFAULT is a declared variable';
is EVAL(Q[use ExportStashOurVar; closure('x')]), 'closure:x', 'an our code variable is a declared routine';
is EVAL(Q[{ use ExportStashOurVar; $answer }]), 42, 'also from a use nested in a block';
