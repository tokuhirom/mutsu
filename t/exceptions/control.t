use Test;
plan 2;

my $handled = "";
try {
    CONTROL { $handled = "ok" }
    next;
    $handled = "bad";
}
is $handled, "ok", "CONTROL handles next signal";

my $no_control = "";
try {
    $no_control = "before";
}
is $no_control, "before", "try without control still runs";
