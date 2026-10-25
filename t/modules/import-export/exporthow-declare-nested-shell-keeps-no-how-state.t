use lib 't/lib';
use Test;

# A declaration nested in another one is registered twice: by a compile-time
# shell that already ran its custom `is` traits, and in place. The HOW the
# in-place registration installs must not inherit the STATE the shell's
# `compose` left in its metaobject (the compose runs once per registration),
# while whatever the shell's traits set must still hold -- the trait does not run
# again (Red's nested `model Ble is table<...>` inside `model Bla`).

use DeclareStateHow;

plan 5;

stately Outer {
    stately Inner is tagged("inner-tag") { }
    stately Other { }
}

is Outer::Inner.^composed-count, 1, "the nested class composed once";
is Outer::Other.^composed-count, 1, "a nested class without a trait composed once";
is Outer.^composed-count, 1, "the outer class composed once";

stately Plain is tagged("plain-tag") { }
is Plain.^composed-count, 1, "a top-level class composed once";
is Plain.^tag, "plain-tag", "its trait ran";
