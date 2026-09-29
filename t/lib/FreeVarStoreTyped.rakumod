unit module FreeVarStoreTyped;

# Fixture for t/routines/free-var-store-ignores-caller-constraint.t: routines
# that assign to their own compunit's file-scope lexicals, the `Test.rakumod`
# `_init_io` shape (#10049).
my $output;
my Int $count = 1;

sub init-output() is export { $output = $PROCESS::OUT; $output.^name }
sub set-count($v) is export { $count = $v; $count }
