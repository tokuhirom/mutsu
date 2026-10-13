use Test;

# From CSS::Module's CSS::Module::CSS3::Values_and_Units: inside a class body,
# `also is Calc` names the `Calc` the body itself declares, not an outer type
# of the same name.

plan 5;

class Calc { method outer { 1 } }

class Actions {
    class Calc { method inner { 2 } }
    also is Calc;
}
is Actions.^mro.map(*.^name).join(' '), 'Actions Actions::Calc Any Mu',
    'bare also-is picks the nested type';
is Actions.new.inner, 2, 'the nested type is the parent';
ok !Actions.new.can('outer'), 'the outer type is not inherited';

class Qualified {
    class Calc { method inner { 3 } }
    also is Qualified::Calc;
}
is Qualified.new.inner, 3, 'qualified also-is names the nested type';

module G {
    class Calc { method outer { 1 } }
    class Holder {
        class Calc { method inner { 4 } }
        also is Calc;
    }
}
is G::Holder.^mro.map(*.^name).join(' '), 'G::Holder G::Holder::Calc Any Mu',
    'a class nested in a module resolves its own nested type';
