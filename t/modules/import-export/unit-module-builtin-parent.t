use Test;
use lib 't/lib';
use UnitModuleBuiltinParent :NOT-SET;

# A class inside a `unit module` that inherits from a built-in type the
# compiler did not know (Nil, Failure, ...) must keep the built-in as its
# parent instead of looking for `<Module>::<Name>`.
plan 7;

is NOT-SET.^mro.map(*.^name).join(","), "UnitModuleBuiltinParent::NOT-SET,Nil,Cool,Any,Mu",
    "is Nil keeps the built-in parent";
ok NOT-SET ~~ Nil, "NOT-SET smartmatches Nil";
is UnitModuleBuiltinParent::MyFail.^parents(:local).map(*.^name).join(","), "Failure",
    "is Failure keeps the built-in parent";

# A module-defined class named like a built-in still wins.
is UnitModuleBuiltinParent::V2.new.hi, "mine", "own class wins over the built-in name";
is UnitModuleBuiltinParent::V2.^mro.map(*.^name).join(","),
    "UnitModuleBuiltinParent::V2,UnitModuleBuiltinParent::Version,Any,Mu", "its MRO names the module's class";

# The inline form behaves the same.
module Inline {
    class Blank is Nil { }
}
is Inline::Blank.^mro.map(*.^name).join(","), "Inline::Blank,Nil,Cool,Any,Mu", "inline module";
is Inline::Blank.^parents(:local).map(*.^name).join(","), "Nil", "inline local parent";
