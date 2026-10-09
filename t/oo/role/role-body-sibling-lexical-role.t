use lib 't/lib';
use Test;
use RoleBodySiblingLexicalRole;

plan 1;

# A parameterized role body that names a sibling `my role` (`my Ser:U $x`)
# runs when a class nested in the declaring module instantiates it lazily.
# Found via Protocol::Postgres (ecosystem).
is RoleBodySiblingLexicalRole::Start.new.go, 'strstr', 'sibling lexical role resolves in a nested package';
