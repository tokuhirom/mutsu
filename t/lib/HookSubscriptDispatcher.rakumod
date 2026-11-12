# Fixture for t/modules/import-export/export-hook-subscript-dispatcher.t
# (#15371): a `sub EXPORT` handing out a dispatcher derived from the built-in
# `postcircumfix:<{ }>`, the way CompUnit::Util's `push-lexical-multi` does.
class SubscriptFoo is export { }
sub EXPORT {
    my $d := &postcircumfix:<{ }>.clone.derive_dispatcher;
    $d.add_dispatchee(sub (SubscriptFoo:D $, Str:D $key) { "win $key" });
    %( '&postcircumfix:<{ }>' => $d )
}
