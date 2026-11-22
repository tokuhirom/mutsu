# A whole-array assignment no longer frees memory under a concurrent reader

`@shared = ...` on a `:=`-bound array that another thread was reading could
make the reader see `NaN`: the assignment refilled the array's node in place
while holding only the cell's mutex, freeing the old buffer, and the reader,
which holds the node's ADR-0068 stripe instead, indexed the freed memory
(#18651, found with ThreadSanitizer). The in-place reassign of a cell's array
or hash now takes the cell's and the node's stripes first, like every other
in-place container writer, so a racing reader sees the old or the new contents
and nothing else.
