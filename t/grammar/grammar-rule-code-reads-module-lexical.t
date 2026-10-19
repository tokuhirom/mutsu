use Test;

# A grammar rule's embedded code (`{ … }`, `<?{ … }>`) closes over the scope
# the rule was declared in. A rule declared in a module reads that module's
# file-scope lexicals, also when the parse runs after the module finished
# loading, and also when the rule is inherited by a grammar declared
# elsewhere (#13915). Checked against rakudo.

plan 5;

use lib 't/lib';
use GrammarRuleModuleLexical;

is grml-parse('('), ')', 'a block in the start rule reads a module hash';
is grml-parse('['), ']', '...and an assertion in a subrule reads a module scalar';
is grml-count(), 5, 'a block in a rule named by :rule';

my $grml-n = 'main';
grammar Derived is GrammarRuleModuleLexicalBase {
    token TOP { <inherited> { make $<inherited>.made ~ " / $grml-n" } }
}
is Derived.parse('y').made, 'base 5 / main',
    'an inherited rule reads its own module, the derived one the program';
is GrammarRuleModuleLexical.parse('(').made, ')', 'a parse from the program itself';
