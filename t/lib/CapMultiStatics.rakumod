module CapMultiStatics {
    my %seen{Any};
    my $total = 0;
    proto sub mark(|) is export(:mark) {*}
    multi sub mark(Hash:D $k) { %seen{$k} //= do { 1 }; %seen.elems }
    proto sub add(|) is export(:mark) {*}
    multi sub add(Int:D $n) { $total += $n; $total }
    our sub count() is export(:mark) { %seen.elems }
    our sub total() is export(:mark) { $total }
}
