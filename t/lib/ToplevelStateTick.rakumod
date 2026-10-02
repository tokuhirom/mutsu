unit module ToplevelStateTick;

our sub tick() { state $n = 0; ++$n }

our sub make-counter() {
    sub inner() { state $m = 0; ++$m }
    &inner
}
