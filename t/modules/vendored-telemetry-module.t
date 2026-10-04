use Test;

# Rakudo's core `Telemetry` module (#9824) is vendored verbatim under
# modules/Rakudo-Core and runs on mutsu unmodified. This pins the runtime
# support it relies on and the module's public surface.

plan 22;

use Telemetry;

# --- the module's public surface ---------------------------------------------

my $t = T;
isa-ok $t, Telemetry, 'T returns a Telemetry snapshot';
ok $t<cpu> >= 0, 'T<cpu> reads a column';
ok T<wallclock> > 0, 'T<key> with no space is a call then a subscript';
is T<max-rss cpu>.elems, 2, 'T<a b> slices two columns';
ok $t.sampler.formats.elems > 5, 'the default sampler reports several columns';

my @t;
snap(@t);
snap(@t);
is @t.elems, 2, 'snap(@t) collects snapshots';
my @p = periods(@t);
is @p.elems, 1, 'periods(@t) gives one period between two snapshots';
isa-ok @p[0], Telemetry::Period, 'a period';
ok @p[0]<wallclock> >= 0, 'a period has a wallclock delta';

my $report = report(@t, :columns<wallclock cpu>);
like $report, /'Telemetry Report of Process'/, 'report(@t) has the header line';
like $report, /'wallclock' \s+ 'cpu'/, 'report header lists the requested columns';

# --- runtime support the module needs ----------------------------------------

# The usage rows are native int arrays read with nqp ops, as Telemetry does.
use nqp;
my $usage := Thread.usage;
is nqp::elems($usage), 6, 'Thread.usage has six columns';
ok nqp::atpos_i($usage, 5) >= 0, 'Thread.usage columns are native ints';
await start { 42 };
my $th = Thread.start({ 1 });
$th.finish;
ok nqp::atpos_i(Thread.usage, 0) >= 1, 'Thread.usage counts a started thread';
ok nqp::atpos_i(Thread.usage, 3) >= 1, 'Thread.usage counts a joined thread';

is nqp::elems($*SCHEDULER.usage), 10, '$*SCHEDULER.usage has ten columns';
is nqp::elems(ThreadPoolScheduler.usage), 10, 'the type object answers too';
ok Kernel.cpu-cores > 0, 'Kernel.cpu-cores works on the type object';

# A native int variable assigned from a native str coerces it.
my str $s = "42";
my int $i = $s;
is $i, 42, 'native str assigned to a native int coerces';

# A hyper subscript inside an interpolated block.
my @rows = [<a b>], [<c d>];
is "{@rows>>.[1].join(',')}", 'b,d', '>>.[..] inside an interpolated block';
my @cols = <a b>;
my %h = a => 'x', b => 'y';
is "%h{@cols}>>.[0].join(',')", 'x,y', 'a hyper subscript after a hash slice interpolates';

# An imported term followed by `<...>` is a call then a subscript.
sub f() { %(key => 7) }
is f<key>, 7, 'f<key> calls f then subscripts';
