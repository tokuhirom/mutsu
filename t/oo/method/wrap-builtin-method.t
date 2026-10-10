use Test;
plan 8;

my $p = Str.^find_method("uc");
my $h = $p.wrap: method (|c) { "W:" ~ callsame };
is "a".uc, "W:A", 'wrapped Str.uc runs the wrapper and the native method';
$p.unwrap($h);
is "a".uc, "A", 'unwrap restores the native method';

DateTime.^find_method('year').wrap(method () { "wrapped" });
is DateTime.new(2020,1,2,3,4,5).year, "wrapped", 'wrapped DateTime.year';

my $t = "";
my $pp = $*OUT.^find_method("print");
my $w = $pp.wrap: method (|c) { $t ~= c.list.join };
print "a"; $*OUT.print("b");
$pp.unwrap($w);
is $t, "ab", 'wrapped IO::Handle.print captures print and .print';
is "x".uc, "X", 'unrelated call unaffected';

# An `is rw` wrapper invocant is the caller's container (#12506).
my $dt = DateTime.new(2020,1,2,3,4,5);
sub timezone(DateTime:D $self is rw) { $self = $self.in-timezone(3600); callsame }
CORE::DateTime.^find_method('timezone').wrap(&timezone);
is $dt.timezone, 3600, 'callsame sees the invocant the wrapper assigned';
is $dt.hour, 4, 'the wrapper wrote back to the caller variable';
my $s = "abc";
sub shout(Str:D $self is rw) { $self = "xyz"; callsame }
Str.^find_method('uc').wrap(&shout);
is $s.uc ~ $s, "XYZxyz", 'rw invocant write-back on a Str variable';
