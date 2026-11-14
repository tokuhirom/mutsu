# Attributes typed by a constant alias: `gpointer[T]` loads, `.type` is the aliased type

Gnome::Cairo 0.3.0 declares `has gpointer[cairo_path_data_t] $.data;`, where
`gpointer` is a sigilless constant aliasing NativeCall's `Pointer` (exported by
Gnome::N::GlibToRakuTypes). The attribute's implicit default is the bareword
`gpointer[cairo_path_data_t]`, and the RakuAST frontend only carried
parameterized type words whose base starts with an uppercase letter, so it
refused the unit — and with it 90 of the 145 modules Gnome::Gtk3 0.49.1
provides (#17672). The finalizer now asks the classifier whether the base of a
`word[args]` bareword names a type or a term, as it already does for a plain
type word, and the EVAL-time undeclared-routine scan looks up such a word by its
base.

Running the distribution then showed that an attribute typed by a *plain*
constant alias (`constant gp = Int; has gp $.d`) kept the alias's name as its
type: `.type` was a package called `gp`, the type check accepted any value, and
a CStruct member typed `gpointer` defaulted to a stray `Pointer` package instead
of NativeCall's. The attribute's type is now resolved through the alias when the
class is declared, the same way the base of a parameterized alias already was,
so `.type` is the aliased type object, as on rakudo.
