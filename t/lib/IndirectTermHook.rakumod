sub EXPORT() {
    sub proxy-value() is raw {
        my $value = 7;
        Proxy.new(
            FETCH => -> $ { $value },
            STORE => -> $, $new { $value = $new },
        )
    }
    Map.new: 'aaa' => proxy-value(), 'Bbb' => proxy-value(),
             'UPPER_X' => 2, 'lower_x' => 1, 'ABC' => 5;
}
