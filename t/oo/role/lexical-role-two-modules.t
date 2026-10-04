use lib 't/lib';
use Test;

# Distribution: App::Rak (highlighter and Needle::Compile each declare their own
# `my role Type`). Two modules that each declare a lexical role of the same name
# are independent: the second load used to continue the first one's role and its
# routines died with "Undeclared name: Type".

use LexRoleTypeA;
use LexRoleTypeB;

plan 3;

is a-who(), "A", "the first module's routine sees its own role";
is b-who(), "B", "the second module's routine sees its own role";
is (b-who() ~ a-who()), "BA", "both stay independent after both are loaded";
