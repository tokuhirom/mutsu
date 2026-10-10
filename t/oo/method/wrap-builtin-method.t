use Test;
plan 5;

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
