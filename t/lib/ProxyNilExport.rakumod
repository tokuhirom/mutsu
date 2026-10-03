# A scalar exported as a Proxy whose STORE deletes on Nil (Env's shape).
sub EXPORT() {
    my $value is default(Nil) = 5;
    my @log;
    my $p := Proxy.new(
        FETCH => -> $ { $value },
        STORE => -> $, \new-value { @log.push: new-value.raku; $value = new-value },
    );
    Map.new: ('$PX' => $p, '@PX-LOG' => @log)
}
