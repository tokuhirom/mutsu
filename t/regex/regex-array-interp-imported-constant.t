use lib $?FILE.IO.parent(2).add('lib');
use Test;
use RegexArrayConstPkg;

# `<@Pkg::name>` naming an `our constant @name` of an imported
# `unit module` was an empty alternation ("Null regex not allowed").
# Found via the Bitcoin distribution (`<@Base58::alphabet>+`).
plan 3;

ok "abc" ~~ /^ <@RegexArrayConstPkg::alphabet>+ $/, 'qualified constant array interpolates';
nok "abd" ~~ /^ <@RegexArrayConstPkg::alphabet>+ $/, 'and rejects others';
subset Checked of Str where /^ <@RegexArrayConstPkg::alphabet>+ $/;
ok "cab" ~~ Checked, 'inside a subset where-clause';
