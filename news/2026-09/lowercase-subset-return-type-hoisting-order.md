# A lowercase-named `subset` used as a `-->` return type no longer collides with sub hoisting

A `sub`'s `--> Type` return-type spec was misclassified as a definite return
*value* (like `--> Nil` or `--> 42`) instead of a type constraint when the
type was a user-declared `subset` whose name starts with a lowercase letter.
Any `return $x` in the body then raised a spurious compile-time error:

```raku
subset ipv6_int of UInt where * < 2**128;
sub foo(Str:D $ip --> ipv6_int) {
    return 42;   # mutsu: "No return arguments allowed when return value
                 #         ipv6_int is already specified in the signature"
                 # raku:  42
}
```

An uppercase-named subset never hit this, because the final fallback
heuristic in `is_definite_return_spec` treats any all-alphanumeric uppercase
name as a type by convention. A lowercase name has no such fallback, so it
depended on the interpreter's own runtime type registry (`self.has_type`) —
which reflects **execution** order, not declaration order. Top-level subs
are hoisted so their names are callable before their textual position, which
means the hoist pre-pass validates a sub's signature before an
earlier-*or*-later `subset` statement has actually run.

Fixed by moving the classification onto the parser's own `is_user_declared_type`
registry instead: it is populated for every `subset`/`class`/`role`/`grammar`/
`enum` declaration during **parsing**, and parsing always completes for the
whole file before any statement executes — so by the time a sub's signature
is validated, the registry already has every subset the file declares,
independent of textual or hoisting order. `Compiler::is_definite_return_spec`
now consults it (mirroring the existing enum-value twin check for #8022), and
that classification is threaded through `CompiledRoutineMetadata` (a new
`is_definite_return_value` field, computed once at plan lowering) so the
runtime's sub-registration check — which used to re-derive the same answer
against the order-dependent live type registry — reads the compile-time
answer instead.

Found while working the `Net::BGP` ecosystem distribution's `Net::BGP::IP`
module (`our subset ipv6_int of UInt where * < 2**128;` used as a return
type), locked on [#7884](https://github.com/tokuhirom/mutsu/issues/7884).

Pin: `t/types/enum-subset/lowercase-subset-return-type.t`.

Closes [#8657](https://github.com/tokuhirom/mutsu/issues/8657).
