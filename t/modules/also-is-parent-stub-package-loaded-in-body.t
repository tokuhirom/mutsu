use Test;
use lib 't/lib-also-is-stub-package';

# `grammar StubPkg::Gram::Ext { use StubPkg::Gram; also is StubPkg::Gram }`
# loaded from a unit whose nested `unit module StubPkg::Gram::Holder::Util`
# had left `StubPkg::Gram` as a package stub: the `also is` parent is resolved
# after the body's `use` has loaded the grammar, not judged a package first
# (CSS::Specification::Compiler: `grammar CSS::Specification::Extended`).

plan 3;

use StubPkg::Gram::Holder;

is ~StubPkg::Gram::Holder.parse-a, 'a', 'the extension inherits the loaded grammar';

throws-like 'package PkgOnly {}; class InheritsPkg is PkgOnly {}',
    X::Inheritance::Unsupported, 'a header parent that is a package is rejected';
throws-like 'package PkgOnly2 {}; class InheritsPkg2 { also is PkgOnly2 }',
    X::Inheritance::Unsupported, 'so is an also-is parent still a package after the body';
