# `Qa::B`, `qw::B`, `Qs::B` are package names, not a quote with a fused adverb

A package or class whose first segment is a quote-language name followed by a
fused-adverb letter (`Qa::B::Foo`, `Qw::B`, `qa::B`, `Qs::B`, `Qf::B`) was read
as the quote construct -- `Q` plus the adverb `a`, with `:` as the delimiter --
so any term using it died "Confused. Two terms in a row". `Q::Name`/`q::Name`
were already exempt, and `::` is never a quote delimiter, so both quote parsers
now refuse a `::` that follows the fused adverbs. Real quotes (`Qw<a b>`,
`Qww<a b>`, `q:w<a b>`) are unchanged.
