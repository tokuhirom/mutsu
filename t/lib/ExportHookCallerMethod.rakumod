sub EXPORT(\DISTRIBUTION) {
    my %META; %META := $_ with try DISTRIBUTION.meta;
    Map.new: 'EXPORTED-VALUE' => (%META<value> // "")
}
