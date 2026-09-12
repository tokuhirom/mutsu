# A user class `is Version` now inherits Version's native positional constructor

`class MyVer is Version { }` died with `Default constructor for 'MyVer' only takes named
arguments` on `MyVer.new("1.2.3")`, because the native-constructor dispatch in
`methods_object_dispatch_new.rs` matched Version's constructor arm on the **literal** dispatch
name `"Version"` — a subclass's dispatch name is its own (`"MyVer"`), so the arm never matched and
fell through to the generic named-arguments-only constructor. This blocked the `Version::Raku` and
`Version::Nginx` distributions, both `class Version::* is Version { ... }` adding only extra
methods and relying on inheriting the positional-string constructor.

Unlike `IO::CatHandle`/`IO::Path` (whose MRO-aware subclass check a few lines above already
handles this), `Version`'s own representation (`ValueView::Version { parts, plus, minus, text }`)
carries no class-name tag to fall back on — building one the same way `Version.new` always has
would leave every subclass instance reporting `.^name` as `"Version"`, and would leave a
subclass's own added methods (`Version::Raku`'s actual reason for existing) unreachable, since
nothing about a bare `ValueView::Version` value can dispatch through `MyVer`'s method table.

A genuine subclass (MRO includes `Version`, dispatch name isn't the literal `"Version"`, no
user-defined `new`) is now built as an ordinary tagged `Instance` — the same `__mutsu_*_value`
native-payload convention `is Int`/`is Str` subclasses already use — carrying the built `Version`
under `__mutsu_version_value`. This gives the instance a real method table (so `Version::Raku`'s
own methods resolve, and `~~ MyVer` / `~~ Version` both work via the ordinary class-MRO
smartmatch, no Version-specific code needed) while `.Str`, `.raku`, `.gist`, and any other method
the subclass does not implement fall through to the real Version's behavior — the same three
call sites (`display.rs`, `gist.rs`, `methods_instance_ops.rs`) already wired for `is Int`/`is Str`.

Plain `Version.new(...)` (the overwhelmingly common case) is untouched: the literal-name arm still
returns a bare `ValueView::Version`, exactly as before.

One gap surfaced but left alone as out of scope, filed as #8134: `@vs.sort` on an array of
`Version` subclass instances loses the "v" prefix when the *sorted Seq itself* is gisted directly
(`say @vs.sort` prints `(1.0.0 2.0.0)` instead of `(v1.0.0 v2.0.0)`), even though the same elements
gist correctly individually, in a `for` loop, or as a plain (unsorted) `Array`. This points at
`sort`'s own internal handling rather than at construction or display, and is unrelated to the
reported symptom.

Fixes #8070.
