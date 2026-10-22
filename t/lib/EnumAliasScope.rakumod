my package Enums {
    our enum Scope (Device => 0, Developer => 1);
}
module EnumAliasScope {
    our constant Scope = Enums::Scope;
    class Device { }
}
