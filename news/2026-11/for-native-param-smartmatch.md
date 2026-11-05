# A `for` loop's native parameter matches its native type

`for 1, 2 -> int $v { say $v ~~ int }` printed `False`; rakudo prints `True`, as it does for a sub, a
pointy-block or an `is copy` parameter. The run-time lookup of the declared type has no entry for a
loop parameter, so the compiler now hands the smartmatch the native type it knows the variable was
declared with (`SmartMatchLhs::Var::native_declared`, from `Compiler::native_declared_type`; a plain
`my int $x` still has none and is read boxed). Single and multi-parameter loops and `is copy` are
covered. Test: `t/nativecall/native-scalar-decl-smartmatch-boxed.t`.
