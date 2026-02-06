use Test;
plan 8;

# die raises an error
my $died = 0;
try {
    die "oops";
    $died = 1;
}
is $died, 0, 'die aborts execution';

# try without CATCH returns Nil on error
my $result = try { die "fail"; 42; };
ok !$result.defined, 'try without CATCH returns Nil on error';

# try without error returns value
my $ok = try { 42; };
is $ok, 42, 'try without error returns block value';

# CATCH with default catches the error
my $caught = "";
try {
    die "an error";
    CATCH {
        default { $caught = "yes"; }
    }
}
is $caught, "yes", 'CATCH default catches the error';

# $! is set to the error message in CATCH
my $msg = "";
try {
    die "specific error";
    CATCH {
        default { $msg = $!; }
    }
}
is $msg, "specific error", '$! contains the error message';

# CATCH with when matching
my $matched = "";
try {
    die "X::AdHoc";
    CATCH {
        when "X::AdHoc" { $matched = "adhoc"; }
        default { $matched = "other"; }
    }
}
is $matched, "adhoc", 'CATCH when matches error message';

# Code after try runs normally
my $after = 0;
try { die "err"; }
$after = 1;
is $after, 1, 'code after try runs normally';

# Nested try
my $inner_caught = "";
try {
    try {
        die "inner";
        CATCH {
            default { $inner_caught = "inner"; }
        }
    }
}
is $inner_caught, "inner", 'nested try/CATCH works';
