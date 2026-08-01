use Test;

plan 9;

# A type declared inside a class body is reachable from that class's own methods
# under its short name, and stays reachable after an unrelated package registers
# a type of the same short name. The short name is inserted into the global env
# at registration time, so without owner-package-chain resolution the later
# registration would steal the bareword from the earlier class's method bodies.

class Outer {
    my grammar Header {
        token TOP { \d+ }
    }

    method parse-it(Str $s) {
        Header.parse($s);
    }
}

package Elsewhere {
    role Header {
        method tag() { "role" }
    }
}

ok Outer.parse-it("123"), "the class's own nested grammar still parses";
is Outer.parse-it("123").Str, "123", "and returns the expected match";
nok Outer.parse-it("abc"), "a non-match is still a non-match";
is Elsewhere::Header.tag, "role", "the unrelated same-named role is unaffected";

# A nested class of the same short name in another class is per-owner: each
# method body sees its own.
class Box {
    class Item {
        method who() { "box-item" }
    }
    method make() { Item.new.who }
}

class Crate {
    class Item {
        method who() { "crate-item" }
    }
    method make() { Item.new.who }
}

is Box.make, "box-item", "Box sees its own nested Item";
is Crate.make, "crate-item", "Crate sees its own nested Item";

# A same-named enum value declared in an unrelated scope must not be displaced by
# the nested type: the enum's owner has no nested type of that name, so the
# bareword resolves to the enum value there.
my enum Expecting <RequestLine Header Body>;

class Consumer {
    method state-of($e) {
        given $e {
            when Header { "header" }
            when Body { "body" }
            default { "other" }
        }
    }
}

is Consumer.state-of(Header), "header", "the enum value wins where no nested type owns the name";
is Consumer.state-of(Body), "body", "the sibling enum value still matches";
is Consumer.state-of(RequestLine), "other", "a non-matching enum value falls through";
