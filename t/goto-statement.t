use Test;
plan 5;

sub nextwith_target { 42 }
sub nextwith_caller {
    &nextwith_target.nextwith();
    0;
}
is(nextwith_caller(), 42, "nextwith returns callee result");

sub caller_line_probe { $?CALLER::LINE }
is(caller_line_probe(), $?LINE, "CALLER::LINE reports direct callsite");

sub caller_line_tail {
    &caller_line_probe.nextwith();
}
is(caller_line_tail(), $?LINE, "nextwith keeps caller callsite");

my $goto_label = 1;
EVAL q{ goto SKIP_LABEL; };
$goto_label = 0;
SKIP_LABEL:
is($goto_label, 1, "goto label jumps forward");

my $goto_expr = 1;
EVAL q{ goto 'SK' ~ 'IP_EXPR'; };
$goto_expr = 0;
SKIP_EXPR:
is($goto_expr, 1, "goto expr jumps to computed label");
