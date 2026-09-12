# Fixture for the slang package-declarator tests (ADR-0091), mirroring the
# shape of Test::Async::Decl: a module whose `sub EXPORT` mixes a grammar role
# into `$*LANG` that adds `package_declarator:sym<...>` candidates, so `use`ing
# it makes two new package declarators available for the rest of the unit.
#
#   widget Name { ... }   is a class built with MetamodelX::WidgetHOW
#   gadget Name { ... }   is a role  built with MetamodelX::GadgetHOW
#
# The two spellings differ deliberately: `widget` names its own `$*PKGDECL` and
# takes its metaclass from a `set_how` in the EXPORT body, while `gadget`
# declares `$*PKGDECL := 'role'` and swaps the metaclass from inside the
# candidate itself — the two shapes Test::Async's `test-hub` and `test-bundle`
# use respectively.
use v6;
use nqp;

class MetamodelX::WidgetHOW is Metamodel::ClassHOW {
    method widget-tag(Mu \type) { 'widget:' ~ type.^name }
}

class MetamodelX::GadgetHOW is Metamodel::ParametricRoleHOW {
    method new_type(|) { callsame }
}

sub EXPORT is raw {
    use NQPHLL:from<NQP>;
    my role DeclGrammar {
        token package_declarator:sym<widget> {
            :my $*OUTERPACKAGE := self.package;
            :my $*PKGDECL := 'widget';
            <sym><.kok> <package_def>
            <.set_braid_from(self)>
        }
        token package_declarator:sym<gadget> {
            :my $*OUTERPACKAGE := self.package;
            :my $*PKGDECL := 'role';
            <sym><.kok>
            { $*LANG.set_how('role', MetamodelX::GadgetHOW); }
            <package_def>
            <.set_braid_from(self)>
        }
    }

    my role DeclActions {
        sub mkey ( Mu $/, Str:D $key ) {
            nqp::atkey(nqp::findmethod($/, 'hash')($/), $key)
        }
        method package_declarator:sym<widget>(Mu $/) {
            $/.make( mkey($/, 'package_def').ast );
        }
        method package_declarator:sym<gadget>(Mu $/) {
            $/.make( mkey($/, 'package_def').ast );
        }
    }

    unless $*LANG.^does( DeclGrammar ) {
        $*LANG.set_how('widget', MetamodelX::WidgetHOW);
        $ = $*LANG.define_slang(
            'MAIN',
            $*LANG.HOW.mixin($*LANG.WHAT, DeclGrammar),
            $*LANG.actions.^mixin(DeclActions)
        );
    }

    Map.new
}
