use Test;

# Pin for the ledger §D slice that dispatches builtin operator-as-function calls
# `infix:<op>(...)` (what &infix:<+>, [+], hyper and reduce lower to) straight to the
# native call_infix_routine handler instead of recording a tree-walk function
# fallback. User-defined operators are resolved before this point, so they still win.

plan 22;

# --- arithmetic infix routines -------------------------------------------------
is infix:<+>(3, 4), 7, 'infix:<+>';
is infix:<->(10, 3), 7, 'infix:<->';
is infix:<*>(6, 7), 42, 'infix:<*>';
is infix:<%>(17, 5), 2, 'infix:<%>';
is infix:<**>(2, 10), 1024, 'infix:<**>';
is infix:<min>(5, 2), 2, 'infix:<min>';
is infix:<max>(5, 2), 5, 'infix:<max>';
is infix:<gcd>(12, 18), 6, 'infix:<gcd>';
is infix:<lcm>(4, 6), 12, 'infix:<lcm>';

# --- comparison / relational ---------------------------------------------------
is infix:<==>(3, 3), True, 'infix:<==>';
is infix:<lt>(2, 5), True, 'infix:<lt>';
is infix:<eq>("a", "a"), True, 'infix:<eq>';
is infix:<ne>(1, 2), True, 'infix:<ne>';

# --- string -------------------------------------------------------------------
is infix:<~>("foo", "bar"), "foobar", 'infix:<~>';
is infix:<x>("ab", 3), "ababab", 'infix:<x>';

# --- through reduce / hyper (lower to the infix routine) -----------------------
is ([+] 1, 2, 3, 4), 10, 'reduce [+]';
is ([*] 1, 2, 3, 4, 5), 120, 'reduce [*]';
is reduce(&infix:<+>, 10, 20, 30), 60, 'reduce(&infix:<+>, …)';
is-deeply ((1, 2, 3) >>+>> 10), (11, 12, 13), 'hyper >>+>>';

# --- minus-sign normalisation (U+2212) -----------------------------------------
is infix:<−>(10, 4), 6, 'infix:<−> (unicode minus) normalises to -';

# --- user-defined operator still takes precedence ------------------------------
sub infix:<plus>($a, $b) { $a + $b + 100 }
is infix:<plus>(1, 2), 103, 'user-defined infix:<plus> wins over native dispatch';
multi infix:<%%%>(Int $a, Int $b) { "custom-$a-$b" }
is infix:<%%%>(3, 4), "custom-3-4", 'user-defined multi operator wins';
