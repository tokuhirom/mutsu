use Test;
# `use lib` runs at BEGIN time even when its argument is a path expression
# rooted at `$?FILE`, so the module it makes loadable is imported while the
# rest of the file is still being parsed: an exported type is known to a
# later signature, and an exported constant is a term (not a listop that
# would swallow a following `!!`). Found via App::Moneymoor's
# `use lib $?FILE.IO.parent.add('lib').Str; use BudgetFixtures;` suites.
use lib $?FILE.IO.parent(2).add('lib').Str;
use UseLibFileRelative;

plan 2;

sub build(--> Widget) { make-widget() }
is build().n, 42, 'an exported class is a type in a later return signature';

my $x = 1;
my $y = $x == ALPHA ?? BETA !! ALPHA;
is $y, 2, 'an exported constant parses as a term inside ?? !!';
