# `for`, list assignment and `|` over a `Range` subclass instance iterate its range

`for Q.new(1, 3)`, `my @a = Q.new(1, 3)`, `.map` and `|Q.new(1, 3)` treated an
instance of `class Q is Range` as one item (and `.map` answered `2..4`). The
instance carries its range like a `Seq` subclass carries its sequence, so these
now read the carried range; `Any`'s list rows no longer answer for it (#15312).
