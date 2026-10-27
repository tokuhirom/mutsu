use NativeCall;
use CArrayImport::Err;
unit class CArrayImport::Child;

# Mirrors Gnome::Glib::Variant: a submethod uses `CArray[Str].new`, and a
# sub called from it declares `my CArray[<imported class>]`.
submethod BUILD(*%o) {
  my $c = CArray[Str].new;
  ($!n) = prs();
}
has $.n;
sub prs() { my CArray[N-GE] $ne .= new(N-GE); $ne.elems }
