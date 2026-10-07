# A written method-local read by a `.map` block no longer resolves to the class body's `my`

`class V { my $enc = "outer"; method b() { my $enc = "inner"; $enc ~= "!"; (1,).map({ $enc }).join } }`
returned `outer`; it now returns `inner!` like Rakudo. A closure now vouches for the creating
routine's own written `my`s that it captures as shared cells (`needs_cell_unvouched_*`), so the
inline map/grep loop skips the class/package body's same-named `my` store for them (#11718).
