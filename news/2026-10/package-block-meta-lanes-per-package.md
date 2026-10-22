# Package block `my %h{Type}` lanes are recorded per package

A `module`/`package` block's typed `@`/`%` lexicals now record their declared
element and key types under the package, hiding the enclosing scope's
same-named lanes while the body runs. Routines entered through the closure
path therefore see their own `my %seen{Any}` and not a calling unit class's
`my %seen{Hash}`, which unblocks `PDF::COS::Util`'s `ast-coerce` (PDF
`cos-type-xref` and `io-serialize` now pass fully).
