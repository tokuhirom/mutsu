# Constructor Nil: `is default(...)`, attributive BUILD params, and `$*COERCION-TYPE`'s value

A follow-up to #9699, which made `Class.new(:attr(Nil))` reset a scalar
attribute to its type default. Three cases were still off:

- **`has $.x is default(42)`** got `Any` instead of `42`. The constructor now
  uses the same Nil rule (`attr_store_nil_default`) as assignment through an
  accessor, and that rule checks the `is default` value first.
- **`submethod BUILD(:$!x)`** with `.new(:x(Nil))` still bound a raw `Nil`. An
  attributive parameter binds by assignment, so both the interpreter binder
  and the VM's fast named-parameter binder now apply the same rule.
- **`$*COERCION-TYPE`** held the bare target class (`C1`). rakudo holds the
  coercion type itself (`C1(Any)`). It is now bound once around the whole
  coercion, so a `COERCE` method sees it as well as the fallback `new`. It is
  bound only for user-class targets, since builtin coercions run no code that
  could observe it.
