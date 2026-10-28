use NativeCall;
unit role RolePointerAttr;

# #15207 (Selkie::App::Internal::ErrorLog): the attribute types are spelled with
# names only THIS file imports (`Pointer`, `CArray` from NativeCall).
has Pointer $!handle;
has CArray $!buf;
has Int $!n = 3;

method describe { "n=$!n handle={$!handle.defined} buf={$!buf.defined}" }
method set-handle { $!handle = Pointer; self }
