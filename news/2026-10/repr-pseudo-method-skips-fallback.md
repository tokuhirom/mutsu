# `.REPR` no longer reaches a class's FALLBACK

`.REPR` is a universal pseudo-method, but the user-`FALLBACK` interception ran before the `REPR`
arm, so `A.REPR` on any class declaring `method FALLBACK` returned `fb:REPR`. Gnome::N's
NativeCall setup does `self.package.REPR`, which made every Gnome::Glib native call die with
"Cannot look up attributes in a ... type object". Eight of Gnome::Glib's ten test files now pass;
the rest are tracked in #13144 and #13145.
