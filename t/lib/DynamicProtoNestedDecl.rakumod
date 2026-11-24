# The proto of `f` declares `my $*CTX` in a nested block (#19174, after Red's
# `red-do`): before that block runs, a callee of the proto must see the
# caller's `$*CTX`, through `$*CTX` and `DYNAMIC::<$*CTX>` alike.
unit module DynamicProtoNestedDecl;
sub ctx()  is export { DYNAMIC::<$*CTX> // 'none' }
sub ctx2() is export { $*CTX // 'none' }
sub run(&block, $c) is export { my $*CTX = $c; block() }
proto f(|c) is export { take ctx() ~ ' ' ~ ctx2(); { my $*CTX = 7; {*} } }
multi f(&b) is export { run &b, 5 }
