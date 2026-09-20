use Test;

# From Air::Plugin::Donate (ecosystem parity): its role declares
# `has Str $.key is required` alongside `multi method new(*%h) { self.bless: |%h }`,
# and its suite asserts that `Air::Plugin::Donate.new` throws. Rakudo enforces
# `is required` in `BUILDALL`, which `bless` runs -- mutsu checked it only on the
# DEFAULT `new` path, so the commonest custom-constructor idiom (a `method new`
# that delegates to `bless`) silently built an object with the attribute unset.
# A TYPED required attribute was the case that hid it: the pre-BUILD seed leaves
# the declared type object there, and the check only recognised `Any` as unset.

plan 9;

class Typed { has Str $.key is required }
dies-ok { Typed.bless }, 'bless enforces a required typed attribute';
dies-ok { Typed.new }, '... and so does the default new';
is Typed.new(:key<v>).key, 'v', '... while a supplied value constructs fine';

class Untyped { has $.key is required }
dies-ok { Untyped.bless }, 'bless enforces a required untyped attribute';

class CustomNew {
    has Str $.k is required;
    method new(*%h) { self.bless: |%h }
}
dies-ok { CustomNew.new }, 'a custom new delegating to bless still enforces it';
is CustomNew.new(:k<x>).k, 'x', '... and constructs when the value is given';

# BUILD is entitled to supply the value, so the check runs after it.
class BuiltInBuild {
    has Str $.k is required;
    submethod BUILD() { $!k = 'from-build' }
    method new() { self.bless }
}
is BuiltInBuild.new.k, 'from-build', 'BUILD may supply a required attribute';

# A role's required attribute, reached through the role's own `new`/`bless` --
# the Air::Plugin::Donate shape, as a pun.
role Req {
    has Str $.key is required;
    multi method new(*%h) { self.bless: |%h }
}
dies-ok { Req.new }, 'a punned role enforces its required attribute';
is Req.new(:key<r>).key, 'r', '... and constructs when it is supplied';
