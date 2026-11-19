# A package-block `my %h` element store goes where its reads look

A routine of a `module M { my %seen; ... }` block reads `%seen` from the
package's static store, ahead of anything its env can see. The element store
(`%seen{$k} = ...`, `//=`, `:delete`) used that store only when the env could
*not* see the name. Since closures resolve a lexical through their capture
before their callers (#18237), a closure's capture can answer `%seen` with a
same-named container of an unrelated caller, picked up when another importer
of the routine was `require`d at run time from that caller's frame. The store
then wrote into that copy while every read kept answering from the package
store, so the write was lost (and the caller's container was changed).

PDF's `PDF::COS::Util::ast-coerce` keeps its cycle detection in such a
`my %seen{Any}`, written inside a `$lock.protect: { ... }` block, so the nightly
ecosystem sweep saw `t/io-serialize.t` overflow the stack and
`t/cos-type-xref.t` fail. The store now seeds the package container under the
same resolution the read uses (`package_scope_lexical_in`), and puts back
whatever the frame bound under the name afterwards. Both PDF files pass again,
as they do on rakudo.
