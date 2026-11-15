sub EXPORT(*@keys) {
    sub as-scalar($name is copy) is raw {
        my $value = 7;
        Proxy.new(
            FETCH => -> $ { $value },
            STORE => -> $, $new { $value = $new },
        )
    }
    Map.new: @keys.map: -> $name { $name => as-scalar($name) }
}
