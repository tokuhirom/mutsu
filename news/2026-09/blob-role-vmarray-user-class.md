# A user class can compose a buffer role over a VMArray body

`class B does Blob[uint8] is repr('VMArray') { ... }` died with "Blob is not
composable, so B cannot compose it" (#9438). PDF's `PDF::IO::Blob` is exactly
`unit class PDF::IO::Blob does Blob[uint8] is repr('VMArray');`, so this blocked
loading most of PDF::Font::Loader.

In mutsu `Blob`/`Buf` are native value types, not registered roles. A buffer is
an instance whose storage attribute holds a contiguous byte node, and whether a
value "is a buffer" was decided from its class name (`Buf[uint8]`, `blob8`,
`utf8`, ...). A class-name decision can serve a user class too, so no new
representation is needed. When a class that `does Blob[..]` / `Buf[..]` is
declared `is repr('VMArray')`, the parent validation records it in a
process-wide table (`value_buf::register_buffer_class`) instead of composing the
role. Its instances are then ordinary buffers that carry the user's class name,
so `.^name`, the user's methods and the native buffer methods all apply.

That only works if every class-name test goes through the shared predicates
(`is_buf_or_blob_class`, `is_blob_like_class`, `is_buf_like_class`) and the
element-type readers (`elem_type`, `buf_elem_width`, `buf_elem_type_name`),
which now consult the table. Twenty-four hand-rolled copies of the predicate
across eighteen files did not, which is why `.bytes`, `.push` and `.subbuf`
failed at first. They now call the shared predicates. A copy that deliberately
left out the encoding buffers (`utf8`, `utf16`) still excludes them, with the
one exception of truthiness: `''.encode.Bool` used to be True, and is now False
as in rakudo.

`class B is Buf` stays unsupported: rakudo rejects it too ("This type (B) does
not support positional operations").

Re-measured with `scripts/ecosystem-sweep.py --only PDF::Font::Loader`, the nine
modules that died on the Blob composition now reach the next blocker ("Unknown
function: accessor", from PDF::COS::Tie's attribute HOW), filed as #9479.

Pin: `t/oo/role/blob-role-vmarray-user-class.t`.
