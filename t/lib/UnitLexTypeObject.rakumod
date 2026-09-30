unit module UnitLexTypeObject;

# File-scope scalars whose value is a type object. A plain (non-`our`) routine
# of the module reads them after the module has finished loading.
my IO::Handle $handle;
my Int $typed;
my Int $typed-ro;
my $untyped;
my $assigned-type = Str;
my ($dest-a, $dest-b);
my Int $defined = 42;

sub read-handle() is export { $handle }
sub read-typed() is export { $typed }
sub read-typed-ro() is export { $typed-ro }
sub read-untyped() is export { $untyped }
sub read-assigned-type() is export { $assigned-type }
sub read-destructured() is export { ($dest-a, $dest-b) }
sub read-defined() is export { $defined }
sub set-typed(Int $v) is export { $typed = $v }
