# `IO()` coerces a `%?RESOURCES` entry

`sub load-stylesheet(IO() $css)` called with `%?RESOURCES<x.css>` died with
"Impossible coercion from 'Distribution::Resource' into 'IO'". A resource answers
`.IO` natively, so the coercion now yields the IO::Path of its file. CSS::TagSet's
`tag-set-pdf.t` gets past construction (the stylesheet parse that follows is a
separate performance problem).
