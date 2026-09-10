use Test;

plan 4;

# A bareword global write (`Red = 5`) that reassigns an enum CONSTANT's own
# binding is illegal and must still raise X::Assignment::RO.
{
    my $threw = False;
    try {
        EVAL 'enum Color <Red Green Blue>; Red = 5;';
        CATCH { default { $threw = True; } }
    }
    ok $threw, 'reassigning an enum constant by its own name still dies';
}

# An ORDINARY variable that merely holds an enum value transiently must
# not be mistaken for that binding. A for-loop's second `.kv` parameter is
# rebound on every iteration (via a bareword global write when it is not
# compiled to a local slot); when a PRIOR iteration's value happened to be
# an enum member, the rebind for the NEXT iteration used to be misread as
# "reassigning the enum constant itself" and raised a spurious
# X::Assignment::RO, even though the loop never touched the constant at
# all -- only its own unrelated loop variable.
enum Color <Red Green Blue>;
for (('b', Red), ('a', 1)) -> ($k, $v) {
    my $x = $v;
    ok True, "iteration for key '$k' completed without dying";
}

# The forward order (enum value bound first, ordinary value second) is the
# one that actually exercises the rebind; run it explicitly so the pin does
# not depend on Hash key iteration order picking it.
{
    my %matcher = b => Red, a => 1;
    my $ok = True;
    for %matcher.kv -> $k, $v {
        my $x = $v;
    }
    CATCH { default { $ok = False; } }
    ok $ok, 'a .kv for-loop over a Hash with an enum value survives a later rebind';
}
