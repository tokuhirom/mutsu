use v6.e.PREVIEW;
use lib $?FILE.IO.parent(3).add('lib');
use Test;

# CORE::v6c / v6d / v6e name one language revision's setting; PseudoStash is
# the one core class 6.e redefines (#18742).

plan 10;

for <c d e> -> $rev {
    is EVAL("CORE::v6{$rev}::CORE-SETTING-REV"), $rev, "CORE::v6{$rev}::CORE-SETTING-REV";
}
is CORE::v6c::PseudoStash.^ver, '6.c', 'CORE::v6c::PseudoStash is 6.c\'s';
is CORE::v6e::PseudoStash.^ver, '6.e', 'CORE::v6e::PseudoStash is 6.e\'s';
is MY::.^ver, '6.e', 'a stash 6.e code builds is 6.e\'s';
is CORE::v6c::Int.^name, 'Int', 'another name is the core one';

proto which-rev(|) {*}
multi which-rev(CORE::v6c::PseudoStash $) { 'c' }
multi which-rev(CORE::v6e::PseudoStash $) { 'e' }
is which-rev(MY::), 'e', 'a 6.e stash dispatches to the v6e candidate';
{
    use SameBodyExportA;
    is which-rev(same-body-who), 'c', 'a 6.c module\'s stash dispatches to the v6c candidate';
}
ok MY:: ~~ CORE::v6e::PseudoStash, 'smartmatch against the revision class';
