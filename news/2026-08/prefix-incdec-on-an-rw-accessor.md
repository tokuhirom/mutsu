# `++$obj.attr` works

```raku
class S { has $.count is rw = 0 }
my $s = S.new;
say ++$s.count;    # raku: 1
                   # mutsu: Cannot resolve caller prefix:<++>(...);
                   #        the parameter requires mutable arguments
```

`$s.count++` and `$s.count += 1` both worked: the postfix increment/decrement
compiler already had a `MethodCall` arm that reads the accessor into a temp,
increments the temp, and writes it back through
`__mutsu_assign_method_lvalue`. The **prefix** forms had no such arm at all —
`++`/`--` handled `Var`, `BareWord`, a declarator in expression position,
`temp $x`, `.=`-assignment and `Index` targets, and everything else fell
through to `__mutsu_incdec_nomatch`.

## Fix

`compile_prefix_incdec_method_lvalue` mirrors the postfix helper but leaves the
*new* value on the stack, and the two `else` fallbacks in `compiler/expr_unary.rs`
now try it before reporting the no-match error.

An accessor on an *element* (`++@a[1].count`) is still unsupported; the postfix
form does not support it either, so the two stay at parity. Noted in the test.

Pinned by `t/prefix-incdec-on-an-rw-accessor.t`.

## Effect

Cro's session middleware writes `content 'text/plain', 'Visit ' ~ ++$session.count`,
so every session route died and answered with an empty body.
`t/http-session-inmemory.rakutest`'s first test passes now (it was failing with
`got: ''`).
