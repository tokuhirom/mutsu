# A mainline CHECK phaser now sees earlier constants

A `constant` is a compile-time value, but the mainline phaser reordering left every
`constant` in the run-time bucket, so `constant p = 7; CHECK say p.is-prime;` failed
with "No such method 'is-prime' for invocant of type 'Any'". Constants that textually
precede a CHECK, with nothing but hoist-safe statements before them, now run after
`BEGIN` and before `CHECK`. A constant that follows a class or an assignment keeps its
source position, so `class Foo {}; constant f = Foo.new;` is unchanged. Closes #9966.
