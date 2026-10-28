use v6;
use Test;

plan 3;

# `class Type::Blob` is stored in the stash of `Type`, but it binds no lexical
# `Blob` inside `role Type`: a `Blob:D` there is the core `Blob` (#15141,
# Protocol::Postgres).
module PP {
    role Type {
        multi method decode(Int, Blob:D $blob) { "role" }
        method accepts(Str:D $s) { "str" }
    }
    class Type::Blob does Type {
        multi method decode(Str, Blob $input) { "class" }
    }
    class Type::Str does Type {
        method plain(Str $s) { "plain" }
    }
}

is PP::Type::Blob.new.decode(1, Blob.new(1)), 'role', 'role method with Blob:D resolves to the core Blob';
is PP::Type::Blob.new.decode("a", Blob.new(1)), 'class', 'class candidate still dispatches';
is PP::Type::Str.new.accepts("x"), 'str', 'a Str:D parameter in the role is the core Str';
