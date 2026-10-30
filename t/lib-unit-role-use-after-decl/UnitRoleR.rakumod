unit role UnitRoleR;
use UnitRoleP;

method in-try { my $s = try fd-dup(2); $s }
method in-try-block { my $s = try { fd-dup(3) }; $s }
method in-catch { try { return fd-dup(4); CATCH { default { return "caught: " ~ .message } } } }
