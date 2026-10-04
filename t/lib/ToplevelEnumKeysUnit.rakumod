# A unit module's own enum keys belong to the module; only an exported enum's
# keys reach the importer. Used by
# t/modules/module-toplevel-enum-keys-off-frame-env.t.
unit module ToplevelEnumKeysUnit;

enum TEKColour <TEK-Red TEK-Green>;
enum TEKDir is export <TEK-Up TEK-Down>;

our sub red() { TEK-Red }
our sub green-closure() { -> { TEK-Green } }

module Inner {
    enum TEKInner <TEK-In-A TEK-In-B>;
    our sub b() { TEK-In-B }
}
