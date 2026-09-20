use v6;
use lib 't/lib';
use Test;
use ImportConstUser;

# A `my sub` that reads a bareword `constant` imported from a sibling module
# (`use Other;`) must resolve it the same way whether it is invoked by its
# declared name or through a first-class code reference (`.map(&lookup)`,
# `my &f = &lookup; f(...)`). Reached via Terminal::WCWidth (ecosystem/):
# `wcwidth`'s `bisearch($ucs, ZERO_WIDTH)` died with "expected Positional but
# got Str (ZERO_WIDTH)" when called through `$str.NFC.map(&wcwidth)` — the
# indirect-call path pushed a block/closure routine frame that never computed
# its `lexical_package` (always `None`, unlike the equivalent named-call
# path), so the imported constant's home package could not be found and the
# bareword fell back to being treated as a literal string of its own name.

plan 2;

is lookup(1), 3, "direct named call sees the sibling module's imported constant";
is (1, 2).map(&lookup).join(","), "3,3",
    "an indirect call via &lookup also resolves the same imported constant";
