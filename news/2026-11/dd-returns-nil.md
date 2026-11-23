# `dd` returns `Nil`

rakudo 2026.11's `dd` returns `Nil`, so `my $r = dd 7` leaves `$r` as `Any`.
mutsu's `dd` returned its first argument, which made `$r` hold `7` (#19056).
It now returns `Nil` too.
