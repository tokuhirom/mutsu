use Test;

plan 7;

# A `proto rule` / `proto regex` in a role used to be parsed as a proto SUB and
# registered as a package-level routine, so the second instantiation of the
# role (a pun, then a composition, or two roles with the same proto name) died
# with X::Redeclaration. It is a proto regex, like `proto token`. Issue #9337.

role RA { proto rule operation { * }; rule operation:sym<plus> { '+' } }
role RB { proto rule operation { * }; rule operation:sym<minus> { '-' } }

is RA.new.^name, 'RA', 'punning a role with a proto rule works';
is RB.new.^name, 'RB', 'a second role declaring the same proto rule can be punned too';

grammar GA does RA { rule TOP { <operation> } }
grammar GB does RB { rule TOP { <operation> } }
is ~GA.parse('+')<operation>, '+', 'the punned role can still be composed into a grammar';
is ~GB.parse('-')<operation>, '-', 'the other role composes with its own candidate';
nok GA.parse('-'), 'the grammars do not share one another\'s candidates';

role RX { proto regex item { * }; regex item:sym<a> { a } }
is RX.new.^name, 'RX', 'a proto regex in a role can be punned';
grammar GX does RX { token TOP { <item>+ } }
is ~GX.parse('aa'), 'aa', '... and composed afterwards';
