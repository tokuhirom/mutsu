# Fixture for t/lang/parsing/slang-meta-object-identity.t (#17415): activates
# the slang of MetaObjectIdentitySlang::Roles the way Slangify does.
use MetaObjectIdentitySlang::Roles;

sub EXPORT() {
    my $LANG := $*LANG;
    $LANG.define_slang('MAIN',
        $LANG.slang_grammar('MAIN').^mixin(MetaObjectIdentitySlang::Roles::Grammar),
        $LANG.slang_actions('MAIN').^mixin(MetaObjectIdentitySlang::Roles::Actions));
    Map.new
}
