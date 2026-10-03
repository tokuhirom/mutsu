# A `.map(-> \x { })` block no longer makes a later `$x is rw` readonly

A `.map`/`.grep`/`.first` callback on the inline loop path runs its body in
the consuming frame's env and restores the names it binds afterwards. A
sigilless parameter (`-> \x`) also writes a readonly marker for `x` there, and
that marker was not among the restored names, so it outlived the loop: a later,
unrelated `-> $x is rw { $x *= 10 }` map block, or `sub g($x is rw)`, then died
"requires mutable arguments" / "Cannot modify an immutable value". The plan now
restores the marker of every sigilless parameter too (#11429).
