use Test;

plan 2;

# `Foo::Bar.new` where `Foo` was never declared as any kind of package used to
# leak the internal dispatcher's own message ("Unknown method value dispatch
# (fallback disabled): new on Foo::Bar") -- a qualified name auto-vivifies as
# a bare Package value at term-resolution time, so `.new` dispatch is where
# mutsu can first tell an undeclared symbol apart from a genuinely missing
# method on a real type. raku reports it as a missing global symbol (#9795).
throws-like { UndeclaredNs9795::Thing.new }, X::AdHoc,
    message => /'Could not find symbol \'&Thing\' in \'GLOBAL::UndeclaredNs9795\''/,
    '.new on a wholly undeclared qualified type names the missing symbol';

# A genuinely known package/class still reports the right "no such method".
package KnownPkg9795 { }
throws-like { KnownPkg9795.frobnicate }, X::Method::NotFound,
    message => /'No such method \'frobnicate\' for invocant of type \'KnownPkg9795\''/,
    'a missing method on a known package is still reported as a missing method';
