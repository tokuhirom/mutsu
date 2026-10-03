use Test;
use lib 't/lib';

# A module's `UNIT::` lists its top-level `my role`/`my class` declarations,
# so a `sub EXPORT` can export them by reading `UNIT::` (highlighter's
# `UNIT::.grep: { .key eq 'Type' || ... }`). A script's `UNIT::` seen from a
# sub lists the script's own lexical types too.

plan 6;

{
    use UnitStashLexicalType;
    ok MY::<Kind>:exists, 'the module exported its my role via UNIT::';
    is ('x' but Kind).kind, 'words', 'the exported role works';
    is greet(), 'hi', 'the routine exported alongside it';
}
{
    use UnitStashLexicalType 'Helper';
    is Helper.help, 'helped', 'a my class looked up as UNIT::{name}';
}

my class Local { }
my role LocalRole { }
sub unit-types() { UNIT::.keys.grep({ $_ eq 'Local' | 'LocalRole' }).sort.List }
is-deeply unit-types(), <Local LocalRole>, 'UNIT:: from a sub lists the script\'s my types';
nok UNIT::<Kind>:exists, 'an importing block\'s import does not reach the file UNIT::';
