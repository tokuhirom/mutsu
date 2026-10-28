# A `multi infix:` imported by an EXPORT hook and a direct `use` no longer recurses

When the same `multi infix:<X>` reached a scope both through a module's
`EXPORT` hook and through a direct `use` of the defining module, a call whose
operands no candidate accepted recursed until "Too deep recursion". The
built-in operator's fallback looked the captured family up as `&infix:<X>` and
dispatched it again after its candidates had already declined. It now skips
that second dispatch and runs the core operator, as rakudo does.
