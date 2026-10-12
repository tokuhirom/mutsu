# Interpolated uninitialized values are named in the warning

`"$s"`, `"%h<k>"` and `"@a[3]"` of an uninitialized value now warn `Use of
uninitialized value $s|%h{'k'}|@a[3] of type Any in string context.`, as
Rakudo does, instead of the generic `value element of type`. The compiler
records the spelling of each interpolation part that is a scalar variable or a
literal-subscript element (`CompiledCode::interp_part_names`); a block part
(`"{Int}"`) stays unnamed, which also matches Rakudo.
