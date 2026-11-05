my class errno { method symbol { "ENOENT" } }
module RawProxyErrno::Unix {
    my $proxy := Proxy.new(FETCH => -> $ { UNIT::errno }, STORE => -> $, $v { });
    my sub errno() is export is raw { $proxy }
}
sub EXPORT() {
    import RawProxyErrno::Unix;
    my %x = '&errno' => &errno;
    %x
}
