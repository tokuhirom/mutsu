unit module PrivateProtoModule;

# A `my`-scoped (non-`our`) proto/multi pair: exported and callable under its
# short name from an importer, but never a package-stash symbol -- a
# package-qualified call to it must stay unresolved, exactly like a plain
# `sub`. See t/proto-package-qualified-visibility.t.
proto sub secret(|) is export {*}
multi sub secret(Int $x) { "int:$x" }
multi sub secret(Str $x) { "str:$x" }

sub wrapper($x) is export { secret($x) }

our proto sub public-proto(|) is export {*}
our multi sub public-proto(Int $x) { "our-int:$x" }
