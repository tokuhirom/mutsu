# Mu's say/print/can/does, Any.are and Array.shape are rows

The interpreter's first by-name method cascade answered several methods for any receiver: `.say`, `.print`, `.put`,
`.note` without arguments, `.return-rw`, `.can`, `.does`, `.are`, `.shape`, `.printf`, `.sprintf` and `.polymod`.
They are now rows of the method table, reached through the receiver's MRO (ADR-11276 §9.136). A receiver whose type
does not provide the method gets `X::Method::NotFound`, as in Rakudo, where the old arms answered anyway:
- `Plain.new.sprintf('%s')`
- `(1, 2).shape`
