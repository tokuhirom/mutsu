use Test;
use lib 't/lib';

plan 3;

# #15246 (Red `t/23-metamodel-model`: `my $*RED-DB = database "SQLite"; ... use
# Red;`): a call written above a top-level `use` whose `multi EXPORT` re-exports
# an inner module's proto reaches the imported routine, not "Routine does not
# have any candidates. Is only the proto defined?". Same shape as
# Red.rakumod / Red::Database; raku accepts it too.
is late-proto("x"), 'proto-x', 'a call above the use reaches the re-exported proto/multi';
is late-proto(21), 42, 'another candidate';
my $r = late-proto "y";
is $r, 'proto-y', 'in a listop call';

use LateImportProtoOuter;
