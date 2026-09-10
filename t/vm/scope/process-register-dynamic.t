use Test;

plan 5;

# `PROCESS::<$name> = value` sets a process-level dynamic variable so a later
# `$*name` resolves to it (used by Rakudo::Internals.REGISTER-DYNAMIC).
PROCESS::<$MY-PROC-VAR> = 42;
is $*MY-PROC-VAR, 42, 'PROCESS::<$x> = v makes $*x resolve';

# The assignment is an expression returning the assigned value.
my $r = (PROCESS::<$RV> = 7);
is $r, 7, 'PROCESS::<$x> = v returns the assigned value';

# Visible across call frames (dynamic scoping).
PROCESS::<$SEEN> = 'yes';
sub check-it { $*SEEN }
is check-it(), 'yes', 'process dynamic var visible inside a sub';

# Rakudo::Internals.REGISTER-DYNAMIC installs a default via its block.
Rakudo::Internals.REGISTER-DYNAMIC: '$*DBI-DEFS', {
    PROCESS::<$DBI-DEFS> = { ConnDefaults => 'x' }
}
is $*DBI-DEFS<ConnDefaults>, 'x', 'REGISTER-DYNAMIC installs a process default';

# REGISTER-DYNAMIC with a block that simply returns a value.
Rakudo::Internals.REGISTER-DYNAMIC: '$*SIMPLE-DEF', { 99 }
is $*SIMPLE-DEF, 99, 'REGISTER-DYNAMIC binds the block result';
