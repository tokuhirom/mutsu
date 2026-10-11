# A sub declared in a block sees that block's lexicals through EVAL

`call { my $x = 'block'; sub g { EVAL q[$x] }; g() }` returned nothing: the
static link that keeps `EVAL` in a routine from seeing its callers' lexicals
(ADR-12529 phase 3) treated `g` as declared at program scope, so its lookup
skipped the block that declares `$x`. A sub declared inside a closure body now
has that body as its lexical outer.

The link itself also became cheaper: it is a flag on the frame instead of a
link stored on the frame's own env tier, so marking a frame allocates
nothing. An experiment that linked every routine frame found one remaining
blocker, the `is rw` parameter writeback, filed as #12641.
