# Grammar and actions for t/lib/LexicalDeclaratorSlang.rakumod (#16888): a
# RakuAST `package-declarator:sym<gizmo>`, as EEL 0.3 adds `event`.
unit module LexicalDeclaratorSlang::Roles;
use nqp;

role Grammar is export {
    token package-declarator:sym<gizmo> {
        'gizmo' <.kok> <package-def('gizmo')> <.set_braid_from(self)>
    }
}

role Actions is export {
    method package-declarator:sym<gizmo>(Mu $/) {
        self.DECLARATOR($/, nqp::atkey($/.hash, 'package-def').ast);
    }
}
