# `[ $gather.map: ... ]` flattens its lazy map/grep pipe

An array literal with a single element that is a finite lazy map/grep pipe (a `.map` or `.grep` over a gather or a lazy list) read only the pipe's still-empty cache and produced `[]`. The literal now pulls the pipe, as `my @a = $pipe` does (Path::Finder's `[ find(...).map: { ... } ]`).
