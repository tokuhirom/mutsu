use v6;
use Test;

plan 11;

# Parse-time static fold of grammar tokens referenced in enumerated char
# classes (`<-restricted +name-sep>`). These pin the fold's soundness: the
# parse cache is per-package and invalidated on token (re)definition.

# 1. Basic fold: negated single-char token + positive multi-char token.
grammar Ident {
    regex TOP  { ^ <name> $ }
    regex name { <-restricted +name-sep>+ }
    token restricted { [':' | '<' | '>' | '(' | ')'] }
    token name-sep   { < :: > }
}
ok Ident.parse('Zef::Distribution'), 'name with :: separator parses';
is ~Ident.parse('Zef::Distribution')<name>, 'Zef::Distribution', 'full name captured';
nok Ident.parse('Zef(Bad)'), 'restricted chars rejected';
ok Ident.parse('Plain'), 'plain name parses';

# 2. Two grammars, same-looking pattern text, DIFFERENT token content —
#    the parse cache must not leak one grammar's fold into the other.
grammar StopAtColon {
    regex TOP { ^ <-stopper>+ $ }
    token stopper { ':' }
}
grammar StopAtComma {
    regex TOP { ^ <-stopper>+ $ }
    token stopper { ',' }
}
ok StopAtColon.parse('a,b'), 'grammar A: comma allowed';
nok StopAtColon.parse('a:b'), 'grammar A: colon rejected';
ok StopAtComma.parse('a:b'), 'grammar B: colon allowed';
nok StopAtComma.parse('a,b'), 'grammar B: comma rejected';

# 3. Inherited grammar overriding the token — the subgrammar's definition
#    must win when parsing through the subgrammar (virtual dispatch).
grammar BaseG {
    regex TOP { ^ <-stopper>+ $ }
    token stopper { 'x' }
}
grammar SubG is BaseG {
    token stopper { 'y' }
}
ok BaseG.parse('aya'), 'base grammar allows y';
ok SubG.parse('axa'), 'subgrammar allows x (override wins)';
nok SubG.parse('aya'), 'subgrammar rejects y (override wins)';

done-testing;
