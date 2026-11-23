# A type capture called with arguments coerces

`T($v)` inside `role R[::T]`, or in a routine with a `::T` parameter, now
coerces through the type `T` is bound to, as rakudo does: `IntBox.new.make("41")`
for `role Typed[::T] { method make($v) { T($v) } }` composed as `Typed[Int]`
gives `41`. mutsu compiled the call as a routine named `T` and died with
"Unknown function: T". The capture is now a term of the body (and a type in a
role body, as it already was in a routine), so `T($v)` calls the bound type,
while `T()` with no arguments stays the coercion type `T(Any)` (#18939).
