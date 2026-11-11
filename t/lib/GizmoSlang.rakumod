# Fixture for t/modules/slang-declarator-in-module-unit.t (#16888): a package
# declarator added the way EEL 0.3 adds `event` -- `$LANG.set_how` names its
# metaclass and the grammar role's `package-declarator:sym<gizmo>` reads the
# rest with the core `package-def`.
use nqp;

class MetamodelX::GizmoHOW is Metamodel::ClassHOW {
    method gizmo-tag(Mu \type) { 'gizmo:' ~ type.^name }
}

role GizmoGrammar {
    token package-declarator:sym<gizmo> {
        'gizmo' <.kok> <package-def('gizmo')> <.set_braid_from(self)>
    }
}

role GizmoActions {
    method package-declarator:sym<gizmo>(Mu $/) {
        self.DECLARATOR($/, nqp::atkey($/.hash, 'package-def').ast);
    }
}

sub EXPORT(|) {
    my $LANG := $*LANG;
    $LANG.set_how('gizmo', MetamodelX::GizmoHOW);
    $LANG.define_slang('MAIN',
        $LANG.slang_grammar('MAIN').^mixin(GizmoGrammar),
        $LANG.slang_actions('MAIN').^mixin(GizmoActions));
    Map.new
}
