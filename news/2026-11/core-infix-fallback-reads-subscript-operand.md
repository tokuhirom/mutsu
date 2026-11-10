# Core operators read a subscripted operand's value when a user `is rw` candidate declines

With `use Red` (or any user `multi infix:<>>(Numeric() $a is rw, ...)`), a subscripted
operand such as `%h<value>` is passed as its container so the `is rw` candidate could
bind it. When no user candidate matched, the core operator received the container cell
itself and answered `False` (`%h<value> > 3`, `grep(*<value> > 3)`). The fallback now
reads the value behind the cell. Found via the Red distribution (`t/038-equal.rakutest`).
