# ADR-0019 Phase F: `Method` introspection objects now answer `.package`

`.^methods`, `.^method_table`, and `collect_class_methods` build `Method` `Instance` objects for
every reported method, but `make_native_method_object`/`make_method_object_with_owner`
(`src/runtime/methods_classhow_method_obj.rs`) never set a `.package` attribute at all -- so
`.package` on any such object read as `Nil`, regardless of the method's actual owner.

Fixed by threading the already-available owner (declaring class, role, or catalog type) through to
a new `.package` attribute:

- A user-declared class method's `.package` is exactly its declaring class; a runtime-mixed-in
  role method's is exactly the role -- both verified byte-for-byte against `raku`.
- A multi method's dispatcher-shaped entry deliberately leaves `.package` unset: real Rakudo
  answers `(Dummy)`, an internal synthetic type mutsu does not model, and guessing a concrete class
  there would be actively wrong. Each individual `.candidates[N]` entry still gets the correct
  owner.
- A native/built-in method's `.package` now defaults to the catalog type it was reported under
  (e.g. `Str.^methods`'s `chars` entry answers `(Str)`). This is not always Rakudo's true
  declaring type (`Str.uc`'s real `.package` is `(Cool)`) -- a deliberate, accepted mechanism-slice
  default per ADR-0019 Phase F box F1's design decision, strictly better than the prior universal
  `Nil` but not a claim of full parity. Closing that gap exactly is separate, tracked follow-up
  work (ADR-0019's F1 box; `news/2026-08/adr0019-f1-f2-introspection-closeout.md`).

`make_method_object`/`make_method_object_with_candidates` were deleted as dead code once their
only callers were switched to call `make_method_object_with_owner` directly with the owner
threaded through. Pinned by `t/classhow-methods-package.t`.
