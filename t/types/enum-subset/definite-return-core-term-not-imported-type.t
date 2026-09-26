use Test;
use lib $?FILE.IO.parent(3).add('lib').Str;

# In a `-->` return spec, `Empty` is the CORE term (a definite return value),
# even when a class whose name ends in `::Empty` has been imported
# (issue #9530, Red::AST::Empty).

use DefRet::AST::Empty;
use DefRetRole;

plan 5;

sub f(--> Empty) { False }
is f().raku, 'Empty', 'sub --> Empty returns Empty with ...::Empty loaded';

class H does DefRetRole { }
is H.acm(Int).raku, 'Empty', 'role method --> Empty returns Empty';

sub n(--> Nil) { 5 }
is n().raku, 'Nil', '--> Nil still returns Nil';

sub k(--> 42) { 5 }
is k(), 42, '--> 42 still returns 42';

is DefRet::AST::Empty.^name, 'DefRet::AST::Empty', 'the class stays reachable by its full name';
