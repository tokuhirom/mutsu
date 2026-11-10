# Fixture for t/lang/parsing/slang-statementlist-pattern-declarator.t (#16888):
# activates the slang of PatternDeclaratorSlang::Roles the way EEL 0.3 does, and
# hands its importer a `&make` of its own.
use nqp;
use PatternDeclaratorSlang::Roles;

sub EXPORT() {
    my $LANG := $*LANG;
    my $grammar = $LANG.slang_grammar('MAIN');
    my $original := $grammar.HOW.find_method($grammar, 'statementlist');
    my &regular = -> Mu $cursor {
        $original ~~ Method
            ?? $original.($cursor)
            !! nqp::dispatch('boot-code', nqp::getattr($original, nqp::what($original), '$!do'),
                nqp::decont($cursor))
    };
    $LANG.define_slang('MAIN',
        $grammar.^mixin(PatternDeclaratorSlang::Roles::Grammar[&regular]),
        $LANG.slang_actions('MAIN').^mixin(PatternDeclaratorSlang::Roles::Actions));
    Map.new('&make' => sub (|) { 'the importer make' })
}
