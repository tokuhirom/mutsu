use Test;
use lib 't/lib-cu-resolution';
use CuResolveWalker;
use CuResolveTypes;

# A module routine that is the first to read `$*CU` sees the types the unit
# imported as resolved imports, not as unresolved names (identical under rakudo).

plan 1;

my %seen;
CHECK %seen = resolutions();
my Imported $x;

is %seen<Imported>, 'RakuAST::Declaration::Import', 'imported type resolves as an import';
