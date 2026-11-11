# Fixture for t/lang/parsing/slang-package-declarator-lexical.t (#16888): adds a
# `gizmo` package declarator with its own metaclass, the way EEL 0.3 adds `event`
# (`set_how` and `define_slang` from `EXPORT`).
use LexicalDeclaratorSlang::Roles;

class MetamodelX::GizmoHOW is Metamodel::ClassHOW {
    method gizmo-tag(Mu \type) { 'gizmo:' ~ type.^name }
}

sub EXPORT() {
    my $LANG := $*LANG;
    $LANG.set_how('gizmo', MetamodelX::GizmoHOW);
    $LANG.define_slang('MAIN',
        $LANG.slang_grammar('MAIN').^mixin(LexicalDeclaratorSlang::Roles::Grammar),
        $LANG.slang_actions('MAIN').^mixin(LexicalDeclaratorSlang::Roles::Actions));
    Map.new
}
