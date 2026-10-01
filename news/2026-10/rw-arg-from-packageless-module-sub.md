# `is rw` arguments from a package-less module's subs

A module file with no package declaration (Color's `Color.rakumod`, which
holds only `class Color`) calling an `is rw` routine from one of its own subs
died once that sub was called from the importer:
`clip-to 0, $_, 255 for @$rgb` reported "Parameter '$v' expects a writable
container (variable) as an argument". The callee has no compiled entry in the
importer's routine table, so the call takes the on-the-fly compile path in
`dispatch_func_call_inner`, and both of its arms ran the binder without the
call site's argument-source names — the only thing that tells it which caller
variable an `is rw` parameter aliases. They now hand the names over like every
other dispatch arm does (mutsu#10520), which gets CSS::TagSet's `tag-set-pdf.t`
past `Color.new(:rgb)`.
