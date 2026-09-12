use Test;

# A user class `is Version` must inherit Version's native positional-string
# constructor (GH #8070). `constructor_dispatch_name` in
# `methods_object_dispatch_new.rs` matched the LITERAL name "Version", so a
# subclass (whose dispatch name is its own, e.g. "MyVer") fell through to the
# default named-arguments-only constructor and died with
# "Default constructor for 'MyVer' only takes named arguments" -- exactly the
# symptom that blocked the `Version::Raku` and `Version::Nginx` distributions,
# both of which are `class Version::* is Version { ... }` adding only extra
# methods.

plan 8;

class MyVer is Version {
    method shout() { "I AM " ~ self.Str.uc }
}

my $v = MyVer.new("1.2.3");
is $v.gist, 'v1.2.3', 'a Version subclass constructs from a positional string';
is $v.Str, '1.2.3', 'and stringifies like a plain Version';
is $v.^name, 'MyVer', 'the built instance is tagged as the actual subclass';
ok $v ~~ MyVer, 'it matches its own subclass';
ok $v ~~ Version, 'and still matches Version';
is $v.shout, 'I AM 1.2.3', "the subclass's own added method resolves";

# Plain `Version.new` is unaffected by the subclass-aware dispatch.
is Version.new("1.2.3").^name, 'Version', 'a plain Version.new is not tagged as a subclass';

# A subclass that defines its own `new` keeps it -- the native positional
# constructor only fills in for a subclass that does not override `new`.
class MyVerCustom is Version {
    method new(|c) { callsame }
}
is MyVerCustom.new("4.5.6").gist, 'v4.5.6', "a subclass's own new(|c) still reaches the native constructor via callsame";
