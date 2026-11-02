use Test;

# #15698: a `use` in an `if` body is scoped to that body; a routine written
# outside it does not see the import, neither while the body runs nor after.

plan 3;

use lib 't/lib';

sub outside { (try ::('BlockGlobalExportType').hi) // 'none' }

my $t = 1;
my $during;
if $t { use BlockGlobalExportType; $during = outside(); }
is $during, 'none', 'a routine written outside the if body does not see the import while it runs';
is outside(), 'none', 'and not after the body';

my $inside;
if $t { use BlockGlobalExportType; $inside = BgeType.hi; }
is $inside, 'bge', 'the body itself sees the import';
