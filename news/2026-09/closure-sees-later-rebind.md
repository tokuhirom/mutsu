# A closure sees a `:=` rebind made after it captured the name

`my $a = 1; my &c = { $a }; $a := 2; say c()` printed `1`; Rakudo prints `2`
(#9237). This happened in the mainline and inside a sub. Plain assignment
(`$a = 2`) already worked.

Rakudo's closure reads the lexical's pad slot, which is the *binding*. mutsu's
closure captured the variable's container cell. A rebind replaced only the
frame's slot, so every closure kept the old container. Writing the new value
through the shared cell would not have worked either: a second name bound
earlier with `my $f := $a` holds the same cell and would have been re-bound
along with `$a` (#9207).

The fix adds one level of indirection, and only where it is needed. The
compiler records the slots that a statement- or expression-level `:=` rebinds
after their declaration (`CompiledCode::rebound_slots`). When a closure captures
one of those slots, the slot gets a *binding cell*: a cell whose content is the
variable's container. The frame and every capturing closure share that binding
cell. A rebind swaps its content. `my $g := $a` binds `$g` to the container
inside it, so `$g` does not follow a later rebind.

The existing read and write chokepoints already handle a cell holding a cell
(the #8759 rw-parameter shape), so reads and writes needed no new code. The
design is written up as ADR-0097 §14. The regression test is
`t/routines/closure/closure-sees-later-rebind.t`.
