unit module ImportConstUser;

use ImportConstTable;

# A `my sub` reaches a sibling module's `is export` constant through its own
# file-scope import, exactly like Terminal::WCWidth's `wcwidth` reaching
# `ZERO_WIDTH`/`WIDE_EASTASIAN` from `Terminal::WCWidth::Tables`.
my sub lookup($x) is export {
    TABLE.elems
}

# Mirrors Terminal::WCWidth's real shape more closely: `wcwidth` reaches the
# imported constant only as an argument it passes to a sibling `my sub`
# (`bisearch`), not directly in its own body (#8905).
my sub bisearch($x, @t) {
    @t.elems
}

my sub lookup-via-helper($x) is export {
    bisearch($x, TABLE)
}
