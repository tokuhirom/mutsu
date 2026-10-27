use Test;
use lib $*PROGRAM.parent(2).add('lib');
use LcExportUser;

# From Needle::Compile (via has-word): a routine imported from a lowercase-named
# module (`use has-word`) was treated as a pragma's import, so an EVAL (or a
# RakuAST node's .EVAL) inside the importing module's routine died with
# "Undeclared routine".
plan 3;
is direct(), 'lc:1', 'the import is callable directly';
is viaeval(), 'lc:2', 'and from an EVAL inside the importing module';
is viaast(), 'lc:3', 'and from a RakuAST call evaluated there';
