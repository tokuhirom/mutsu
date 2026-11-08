use Test;
use lib 't/lib';
use MONKEY-SEE-NO-EVAL;

plan 4;

# A `use` of a `sub EXPORT` hook module after a BEGIN block is validated with
# the names the hook returned, as one before it is.
is-deeply (EVAL 'BEGIN { 1 }; use EvalExportHookTerm <U>; U').raku, (EVAL 'use EvalExportHookTerm <U>; U').raku,
    'a hook term is declared after a BEGIN';
throws-like { EVAL 'BEGIN { 1 }; use EvalExportHookTerm <U>; nosuch' }, X::Undeclared::Symbols,
    'an unknown bareword after the BEGIN is still undeclared';
throws-like { EVAL 'use EvalExportHookTerm <U>; nosuch' }, X::Undeclared::Symbols,
    '... as it is without the BEGIN';
is EVAL('BEGIN { 1 }; use ChainExportInner; chain-inner()'), 'inner', 'a hook routine is declared after a BEGIN';
