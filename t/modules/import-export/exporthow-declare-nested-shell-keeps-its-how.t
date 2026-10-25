use lib 't/lib';
use Test;

# A declaration nested in another one, or written as a term, is registered twice:
# by a compile-time shell that already ran its custom `is` traits, and in place.
# The metaobject must stay the shell's: what a trait stored in it (a tag, Red's
# `is table<...>`) must still be there, and `compose` must still have run once.

use DeclareStateHow;

plan 7;

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

given stately Term is tagged("term-tag") { } {
    is .^tag, "term-tag", "a term-form class keeps the state its trait stored in the metaobject";
    is .^composed-count, 1, "and composed once";
}
