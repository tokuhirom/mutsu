# A lowercase `repr('CPointer')` class is a native handle type — `Archive::Libarchive::Raw` goes 1/6 to 5/6

`docs/batteries/compression.md` filed this as the single highest-leverage
NativeCall fix in that survey: `Archive::Libarchive::Raw` declares

```raku
class archive is repr('CPointer') is export { * }            # libarchive's opaque handle
sub archive_read_new(--> archive) is native(LIB) is export { * }
sub archive_read_support_filter_all(archive --> int32) is native(LIB) is export { * }
```

and under mutsu the *second* call died with

```
Type check failed for return value; expected int32 but got Whatever (*)
```

## Verified against the real dist, not a reconstruction

A later survey pass marked this ticket "possibly already fixed" because a
hand-written reconstruction behaved identically to `raku`. It was not fixed —
the reconstruction had simply used an uppercase class name. The dist itself
(REA `Archive::Libarchive::Raw` v0.1.5, fetched and run against the system
`libarchive.so.13`) reproduced the reported failure exactly. Two independent
causes were behind it.

## Root cause 1: the class-name shape heuristic ignored two of the three repr sets

`is_native_struct_type` decides whether an unmapped parameter/return type name is
an opaque native handle to be passed by pointer. It consulted the registry's
`cstruct_classes` only, then fell back to "starts with an uppercase letter or is
package-qualified". libarchive's handle is `class archive` — lowercase,
unqualified, and registered in `cpointer_classes`. So it was rejected, the whole
`sub` declaration skipped native registration, and the `{ * }` stub body ran
instead — returning `Whatever`, which then failed the sub's own `--> int32`
return check. The error blamed the return type of a function that had never been
called.

It now consults `is_native_handle_class`, which already covered all three repr
sets (`CStruct`, `CPointer`, `CUnion`) with the same short-name matching. The
uppercase/qualified shape heuristic stays as the fallback for a class declared in
a compilation unit this registry cannot see.

## Root cause 2: the `(name, version)` library spelling

`constant LIB = ('archive', v13)` was stringified whole into `libarchive 13.so`.
That is fixed separately — see
[`nativecall-cglobal-undefined-str-library-mistokenized.md`](nativecall-cglobal-undefined-str-library-mistokenized.md).

## Measured result

`Archive::Libarchive::Raw` v0.1.5's own test suite, run against system
libarchive 3.7.2:

| | raku | mutsu before | mutsu after |
| --- | --- | --- | --- |
| files fully passing | 6/6 | 1/6 | **5/6** |

The one remaining file, `t/05-archive-read-disk.rakutest`, needs two things this
change does not provide: NativeCall **callbacks** (`archive_write_open($w, 42,
&archive-open, &archive-write, &archive-close)`), and `$*USER` / `$*GROUP`, which
mutsu does not implement at all. Both are filed as their own tickets
([`nativecall-callback-parameter-marshalling.md`](../../todo/tickets/nativecall-callback-parameter-marshalling.md),
[`user-group-dynamic-variables-missing.md`](../../todo/tickets/user-group-dynamic-variables-missing.md)).

## Pin

`t/nativecall-pointer-and-cglobal.t` declares a lowercase
`class fh is repr('CPointer')`, has one native sub return it (`tmpfile`) and a
second take it (`fclose`), using only POSIX symbols present on Linux and macOS
and closing the handle it opens.
