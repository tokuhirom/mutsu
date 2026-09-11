use v6;
use Test;

# A `grammar` declarator with no `is` clause carries an implicit `Grammar`
# parent. An `also is Base` in the body REPLACES it instead of adding a second
# parent, exactly as `grammar G is Base { }` would: keeping both makes the C3
# merge inconsistent whenever `Base` is itself a grammar.
#
# From CSS::Grammar::CSS21 (`unit grammar CSS::Grammar::CSS21; use CSS::Grammar;
# also is CSS::Grammar;`), whose load died with "Inconsistent class hierarchy for
# CSS::Grammar::CSS21".

plan 6;

grammar Base { token foo { 'x' } }
grammar Sub { also is Base; }

ok Sub.parse('x', :rule<foo>).defined, 'block-form grammar inherits via `also is`';
is Sub.^mro.map(*.^name).join(' '), 'Sub Base Grammar Match Capture Cool Any Mu',
    'the implicit Grammar parent is replaced, not duplicated';

class Plain { method zz { 7 } }
grammar Declared is Plain { }
grammar Also { also is Plain; }
is Also.^mro.map(*.^name).skip(1).join(' '), Declared.^mro.map(*.^name).skip(1).join(' '),
    '`also is` linearizes exactly like the declarator form';
is Also.new.zz, 7, 'a non-grammar parent reached through `also is` still dispatches';

grammar Roled does Positional { }
ok Roled.^mro.map(*.^name).grep('Grammar'), 'a `does` role leaves the implicit Grammar parent alone';

# The `unit grammar G; also is Base;` form gathers its parent from a trailing
# statement rather than from the declarator, and must drop the implicit parent
# just the same. `t/lib/AlsoIsUnitGrammar.rakumod` is that form.
use lib $?FILE.IO.parent(2).add('lib').Str;
use AlsoIsUnitGrammar;
ok AlsoIsUnitGrammar.parse('y', :rule<bar>).defined,
    '`unit grammar ...; also is Base;` inherits its base grammar';
