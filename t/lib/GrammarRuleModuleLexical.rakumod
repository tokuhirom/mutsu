my $grml-n = 5;
my %grml-pairs = '(' => ')', '[' => ']';

grammar GrammarRuleModuleLexical {
    token TOP { <opener> { make %grml-pairs{~$<opener>} } }
    token opener { <[(\[]> <?{ $grml-n == 5 }> }
    token count { x { make $grml-n } }
}

grammar GrammarRuleModuleLexicalBase is export {
    token inherited { y { make "base $grml-n" } }
}

sub grml-parse($s) is export { GrammarRuleModuleLexical.parse($s).made }
sub grml-count() is export { GrammarRuleModuleLexical.parse('x', :rule<count>).made }
