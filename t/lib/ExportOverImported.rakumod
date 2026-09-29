use ExportOverBase;

# Re-exports `&shout` over the `&shout` it imported itself (JSON::Pretty over
# JSON::Fast's `to-json`). The multi candidate below calls the bare `shout`,
# which in THIS unit is the imported one, not the override it is exported as.
my proto wrapped($, :$k) {*}
multi wrapped(Cool:D $d, :$k) { '<' ~ shout($d) ~ '>' }
multi wrapped(Mu:U $, :$k) { 'null' }

sub EXPORT() {
    BEGIN Map.new: '&shout' => &wrapped;
}
