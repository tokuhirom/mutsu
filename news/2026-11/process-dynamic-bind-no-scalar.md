# `PROCESS::<$X> := value` binds the value, not a Scalar

`PROCESS::<$R> := 1..3` left a Scalar around the range: `$*R.VAR.^name` was `Scalar` and
`my @a = $*R` kept the range whole (`[1..3,]`), where rakudo answers `Range` and `[1, 2, 3]`. A bind
now stores the value as it is in the process stash (`store_process_dynamic`) and marks the dynamic's
name as having no container, which `.VAR` and `@`-assignment (`OpCode::ItemizeVar`) read from any
frame. An assignment (`PROCESS::<$X> = v`) still makes a Scalar. Still different: rakudo refuses a
later assignment to a bound immutable (`Cannot assign to an immutable value`). Found by
POSIX::PWDENT, whose `$*PWDENT` / `$*GRPENT` are Iterable instances bound this way. Test:
`t/vm/scope/process-dynamic-bind-no-scalar.t`.
