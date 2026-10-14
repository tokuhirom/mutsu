use Test;

plan 5;

# A CATCH that handles the exception still leaves it in `$!` (rakudo behavior).
try { CATCH { default { } }; die "h2" };
is $!.message, "h2", "try with a handling CATCH sets \$!";

sub handled { try { CATCH { default { } }; die "h" }; $!.message }
is handled(), "h", "...inside a routine";

sub plain-block { { CATCH { default { } }; die "g" }; $!.message }
is plain-block(), "g", "a CATCH in a plain block sets \$! too";

try { die "a" };
{ CATCH { default { } }; die "b" };
is $!.message, "b", "a handled exception replaces the earlier \$!";

try { CATCH { when X::AdHoc { } }; die "x" };
is $!.^name, "X::AdHoc", "a `when` that handles it sets \$!";
