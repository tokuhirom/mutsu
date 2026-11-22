unit module WorkerOnce;

# Counts how many times this mainline runs in the process.
our $runs;
$runs++;

sub runs is export { $runs }
