use Test;

# A `my %*ENV` is the environment of its dynamic scope even after the process
# `%*ENV` has been written: reads, callees and child processes see it
# (#17513: Red provisions a test database with `my %*ENV = %environment` and
# its child processes lost the connection settings).

plan 6;

%*ENV<MUTSU_T_GLOBAL> = 'process';

# Red captures the environment before declaring the dynamic one.
sub scoped(%environment) {
    my %*ENV = %environment;
    %*ENV<MUTSU_T_SCOPED> = 'dynamic';
    is %*ENV<MUTSU_T_SCOPED>, 'dynamic', 'read in its own scope';
    is %*ENV<MUTSU_T_GLOBAL>, 'process', 'and it starts from the captured environment';
    callee();
    my $child = run $*EXECUTABLE, '-e', 'print %*ENV<MUTSU_T_SCOPED> // "none"', :out;
    is $child.out.slurp-rest, 'dynamic', 'a child process gets the dynamic environment';
}
sub callee { is %*ENV<MUTSU_T_SCOPED>, 'dynamic', 'a callee sees it' }
scoped(%*ENV.Hash);

nok %*ENV<MUTSU_T_SCOPED>:exists, 'the process environment is untouched';
my $child = run $*EXECUTABLE, '-e', 'print %*ENV<MUTSU_T_SCOPED> // "none"', :out;
is $child.out.slurp-rest, 'none', 'and so is a later child process';
