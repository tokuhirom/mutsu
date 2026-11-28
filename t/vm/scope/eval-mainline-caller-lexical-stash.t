use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 1;

is EVAL(q[sub hidden {}; CALLER::LEXICAL::.AT-KEY('&hidden').defined].AST),
    False, 'an EVAL mainline has no calling routine whose pad contains its sub';
