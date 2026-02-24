use Test;
use Test::Tap;

plan 6;

dies-ok { Supply.schedule-on($*SCHEDULER) },
  "Supply.schedule-on cannot be called on the type object";

my $scheduler = CurrentThreadScheduler.new;
ok $scheduler ~~ Scheduler, "CurrentThreadScheduler matches Scheduler";

my $master = Supplier.new;
my $scheduled = $master.Supply.schedule-on($scheduler);
isa-ok $scheduled, Supply, "schedule-on returns a Supply";
is $scheduled.live, True, "schedule-on preserves liveness";

tap-ok $scheduled,
  [1, 2, 3],
  "schedule-on preserves emitted values",
  :live,
  :after-tap({
      $master.emit(1);
      $master.emit(2);
      $master.emit(3);
      $master.done;
  });

dies-ok { Supply.from-list(1).schedule-on(42) },
  "schedule-on requires a Scheduler argument";
