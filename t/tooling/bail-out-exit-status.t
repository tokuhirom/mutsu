use Test;

# Rakudo's `bail-out` ends the process with status 255 (`Test.rakumod` does
# `exit 255` right after emitting the "Bail out!" line). mutsu emitted the line
# but exited 0, so `prove` -- and `Test::Util`'s `is_run ..., :255status` --
# read a bailing-out file as a clean run.

plan 4;

sub run-snippet(Str:D $code) {
    my $proc = run $*EXECUTABLE.absolute, '-e', $code, :out, :err;
    my $out = $proc.out.slurp(:close);
    $proc.err.slurp(:close);
    ($proc.exitcode, $out);
}

my ($status, $out) = run-snippet 'use Test; ok 1, "runs"; bail-out; done-testing';
is $status, 255, 'bail-out exits 255';
is $out, "ok 1 - runs\nBail out!\n", '... after emitting the assertions and the bail line';

($status, $out) = run-snippet 'use Test; ok 1, "runs"; bail-out "some reason";';
is $status, 255, 'bail-out with a description exits 255 too';
is $out, "ok 1 - runs\nBail out! some reason\n", '... and names the reason';
