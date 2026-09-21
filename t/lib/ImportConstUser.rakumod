unit module ImportConstUser;

use ImportConstTable;

# A `my sub` reaches a sibling module's `is export` constant through its own
# file-scope import, exactly like Terminal::WCWidth's `wcwidth` reaching
# `ZERO_WIDTH`/`WIDE_EASTASIAN` from `Terminal::WCWidth::Tables`.
my sub lookup($x) is export {
    TABLE.elems
}
