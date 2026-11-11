# Fixture for t/lang/parsing/slang-declarator-dynvar-nested-hook.t (#16888):
# activates the slang of DeclaratorDynvarSlang::Roles the way EEL 0.3 does.
use nqp;
use DeclaratorDynvarSlang::Roles;

sub EXPORT() {
    my $LANG := $*LANG;
    my $grammar = $LANG.slang_grammar('MAIN');
    sub adapter($name) {
        my $method := $grammar.HOW.find_method($grammar, $name);
        -> Mu $cursor, |args {
            $method ~~ Method ?? $method.($cursor, |args)
                !! nqp::dispatch('boot-code', nqp::getattr($method, nqp::what($method), '$!do'),
                    nqp::decont($cursor), |args)
        }
    }
    $LANG.define_slang('MAIN',
        $grammar.^mixin(DeclaratorDynvarSlang::Roles::Grammar[
            adapter('statementlist'), adapter('arglist'), adapter('circumfix:sym<( )>')]),
        $LANG.slang_actions('MAIN').^mixin(DeclaratorDynvarSlang::Roles::Actions));
    Map.new
}
