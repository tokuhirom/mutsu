# A conditional use opens its lexical import set

`use X:if(COND)` inside a module now opens a lexical import set like an unconditional `use`, so the module's own subs resolve a bare-word call of an imported routine (`errno.symbol` in Universal::Errno `check-errno`). Universal::Errno `t/traits.t6` passes 29/29.
