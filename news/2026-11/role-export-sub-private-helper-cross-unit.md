# An exported role sub keeps reaching its private helpers when a class from another unit composes the role

A role body's `is export` sub that calls a private `sub` of the same body
(`Storable::Lite`'s `to-file` calling `serialize`) died with `Unknown function`
once the role was composed into a class declared in a different compunit
(`Shareable`). Composition re-declared the exported sub in the composer's
package, out of reach of the role's own helpers; it now runs in the role's
package like every other plain body statement. Shareable's `t/01-methods.t`
now passes (6/6, as on rakudo).
