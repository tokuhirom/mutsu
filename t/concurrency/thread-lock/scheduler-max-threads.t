use Test;

plan 2;

# Regression from the Uxmal ecosystem suite: ThreadPoolScheduler exposes the
# cap already used by mutsu's shared worker pool.
my $expected = $*SCHEDULER.max_threads;
ok $expected > 0, 'the process scheduler reports a positive worker cap';
is ThreadPoolScheduler.new.max_threads, $expected,
    'a ThreadPoolScheduler instance reports its worker cap';
