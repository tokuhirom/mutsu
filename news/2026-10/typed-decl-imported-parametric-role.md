# `my R[T] $x .= new(...)` resolves an imported parameterized role

`my LinearArray[Point3D] $lum .= new-from-pointer(...)` died with "No such method
'new-from-pointer' for invocant of type 'LinearArray[Point3D]'" when
`LinearArray` was a role imported from a module (NativeHelpers::CStruct). Two
places looked up the bare spelling: the declaration's type object (`.^name` said
`LinearArray[Point3D]` where raku says `NativeHelpers::CStruct::LinearArray[Point3D]`)
and the `.=` invocant bareword `R[T]`. Both now resolve the base through the
importer's name, and the invocant becomes the same parameterization the subscript
form `R[T]` builds. With this, `NativeHelpers::Blob`'s `t/02-cstruct.rakutest` runs
all 34 assertions; the file as shipped still dies in its own helper on
`$*VM.name`, recorded as an accepted divergence next to LibraryMake (#12829).
