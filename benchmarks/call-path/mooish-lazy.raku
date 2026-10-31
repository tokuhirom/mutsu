# Call-path harness input (ADR-15737 slice 1, #15688): AttrX::Mooish `is mooish(:lazy)`
# construction plus first read. Needs the real module: scripts/call-path-ab.sh
# fetches AttrX::Mooish 1.0.10 into tmp/call-path/mooish and passes
# `-I tmp/call-path/mooish/lib`; run by hand with the same -I.
#
#   CALLPATH_N=<iterations>  (default 20)
use AttrX::Mooish;

my class Lazy {
    has Int $.foo is mooish(:lazy);
    method build-foo { 42 }
}

my $n = (%*ENV<CALLPATH_N> // 20).Int;
my $sum = 0;
for ^$n {
    my $i = Lazy.new;
    $sum += $i.foo;
}
say $sum;
