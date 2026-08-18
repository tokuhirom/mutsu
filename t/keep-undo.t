use Test;
plan 9;

my $ok = "";
sub succeeds() {
    KEEP { $ok ~= "K"; }
    UNDO { $ok ~= "U"; }
    42;
}
is succeeds(), 42, "routine returns value";
is $ok, "K", "KEEP runs on successful routine exit";

my $ng = "";
my $caught = False;
sub fails() {
    KEEP { $ng ~= "K"; }
    UNDO { $ng ~= "U"; }
    die "boom";
}
try {
    fails();
    CATCH {
        default { $caught = True; }
    }
}
ok $caught, "routine die is catchable";
is $ng, "U", "UNDO runs on exceptional routine exit";

my $block_ok = "";
{
    KEEP { $block_ok ~= "K"; }
    UNDO { $block_ok ~= "U"; }
    1;
}
is $block_ok, "K", "KEEP runs on successful block exit";

my $block_ng = "";
try {
    {
        KEEP { $block_ng ~= "K"; }
        UNDO { $block_ng ~= "U"; }
        die "boom";
    }
    CATCH {
        default { }
    }
}
is $block_ng, "U", "UNDO runs on exceptional block exit";

# KEEP/UNDO on normal completion are decided by the trailing value's
# DEFINEDNESS, not its truthiness -- verified against real raku. See
# todo/tickets/keep-undo-decided-by-value-truthiness-not-completion.md.
my $falsy_defined = "";
{
    KEEP { $falsy_defined ~= "K"; }
    UNDO { $falsy_defined ~= "U"; }
    0;
}
is $falsy_defined, "K", "KEEP runs for a falsy-but-defined trailing value (0)";

my $undefined = "";
{
    KEEP { $undefined ~= "K"; }
    UNDO { $undefined ~= "U"; }
    Any;
}
is $undefined, "U", "UNDO runs for an undefined (Any) trailing value";

# A named routine returning a falsy-but-defined value via `return` also
# runs KEEP, not UNDO.
my $ret_falsy = "";
sub returns-falsy() {
    KEEP { $ret_falsy ~= "K"; }
    UNDO { $ret_falsy ~= "U"; }
    return 0;
}
returns-falsy();
is $ret_falsy, "K", "KEEP runs for a falsy-but-defined `return` value (0)";
