use Test;

# The `R` (reverse) metaop on a reduction reverses the entire fold:
# `[R op] @list` == `[op] @list.reverse`. Previously mutsu swapped operands
# per-step, which is wrong for non-commutative / non-associative ops:
# `[R/] 100,10,2` gave `100/(10/2)` = 20 instead of `2/10/100` = 0.002.
#
# Also covers the single-element reduce bug exposed by the R fix: `[op] $x`
# returns the (numified) element for ANY op, not `op(identity, $x)`.

plan 18;

# [R op] == [op] reversed.
is ([R-] 1, 2, 3, 4), -2, '[R-] reverses the fold (4-3-2-1)';
is ([R/] 100, 10, 2), 0.002, '[R/] reverses the fold (2/10/100)';
is ([R-] 10, 1), -9, '[R-] two elements';
is ([R~] 1, 2, 3), '321', '[R~] reverses concatenation';
is-deeply ([R,] 1, 2, 3), (3, 2, 1), '[R,] reverses the list';
is ([R**] 2, 3, 4), 262144, '[R**] reverses then right-folds (4**(3**2))';

# Scan form [\R op] also reverses first.
is-deeply ([\R-] 1, 2, 3, 4), (4, 1, -1, -2), '[\R-] scan over reversed list';
is-deeply ([\R/] 100, 10, 2), (2, 0.2, 0.002), '[\R/] scan over reversed list';

# Commutative ops are unaffected by the reversal.
is ([Rmin] 3, 1, 2), 1, '[Rmin] (commutative)';
is ([R+] 1, 2, 3), 6, '[R+] (commutative)';

# Single-element reduce returns the element itself (numified), not op(id, x).
is ([-] 5), 5, '[-] single element is the element';
is ([/] 5), 5, '[/] single element is the element';
is ([**] 5), 5, '[**] single element is the element';
is ([%] 5), 5, '[%] single element is the element';
is ([R-] 5), 5, '[R-] single element is the element';

# Single-element reduce still numifies (and validates) the element.
is ([+] "2").WHAT.gist, '(Int)', '[+] "2" numifies to Int';
is ([-] "5"), 5, '[-] "5" numifies to 5 (not -5)';
dies-ok { [+] "hello" }, '[+] on a non-numeric string still dies';
