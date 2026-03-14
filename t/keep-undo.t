use Test;
plan 6;

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
