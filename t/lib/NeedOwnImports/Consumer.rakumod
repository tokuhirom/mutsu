use NeedOwnImports::Provider;

unit class NeedOwnImports::Consumer;

# The point of the fixture: this method resolves a routine THIS compunit
# imported. Nothing the loader does may take it away.
method go() { provided(42) }

sub go-sub() is export { provided(7) }
