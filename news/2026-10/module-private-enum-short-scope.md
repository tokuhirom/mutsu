# Keep short enum member names in their module

An unexported enum member's `E::K` spelling now belongs to its declaring
module's scope. Code in that module can still read it, while an importer sees
the member through its package-qualified spelling only.
