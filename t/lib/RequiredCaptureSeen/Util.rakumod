# A package block whose routines keep their state in package-block `my`
# containers, written from inside a closure (as PDF::COS::Util does).
module RequiredCaptureSeen::Util {
    my %seen;
    my @log;
    proto sub note-key(|) is export(:note-key) {*}
    multi sub note-key($k) {
        my $v = { %seen{$k} //= 42 }();
        { @log[@log.elems] = $k }();
        %seen.elems ~ '/' ~ @log.elems;
    }
}
