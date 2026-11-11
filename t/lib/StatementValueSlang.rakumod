# Fixture for t/lang/parsing/slang-statement-control-value.t (#17460):
# activates the slang of StatementValueSlang::Roles the way Slangify does, and
# hands the importer the slang's runtime helpers.
use StatementValueSlang::Roles;

sub EXPORT() {
    my $LANG := $*LANG;
    $LANG.define_slang('MAIN',
        $LANG.slang_grammar('MAIN').^mixin(StatementValueSlang::Roles::Grammar),
        $LANG.slang_actions('MAIN').^mixin(StatementValueSlang::Roles::Actions));
    Map.new(
        '&with-root'  => &StatementValueSlang::Roles::with-root,
        '&root'       => &StatementValueSlang::Roles::root,
        '&kept-molds' => &StatementValueSlang::Roles::kept-molds,
    )
}
