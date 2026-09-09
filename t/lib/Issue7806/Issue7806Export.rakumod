use NativeCall;

# Mirrors NativeLibs' own shape: `use`s NativeCall, then a custom `sub EXPORT`
# passes the transitively-`use`d package through as a value in its returned
# Map, and `unit module` comes AFTER those two statements (as it does in
# NativeLibs.pm6). See https://github.com/tokuhirom/mutsu/issues/7806.
sub EXPORT(|) {
    Map.new('NativeCall' => NativeCall)
}
unit module Issue7806Export;
