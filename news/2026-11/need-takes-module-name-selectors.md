# `need` takes the version selectors on a module name

`need Compress::Zlib::Raw:ver<1.1.0+>;` failed to compile with "Confused. Two
terms in a row": the `need` statement read the module name and stopped, so
the `:ver<...>` adverb on it was left over as a second term on the same line.
`need` now consumes the `:ver`, `:v`, `:auth` and `:api` selectors on the name
in both their `<...>` and `(...)` forms, as `use` does. Compress::Zlib does
exactly this, and everything that loads it — PDF, PDF::Class, PDF::Content,
PDF::Font::Loader, Pod::To::PDF::Lite, FDF, HTTP::Core, Image::PNG::Portable,
Archive::SimpleZip and more — loads again.
