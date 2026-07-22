use Test;

plan 7;

# A custom `sub EXPORT` that installs an operator sub via a Map — the operator's
# `&infix:<op>` reference must be captured as a first-class Sub that outlives the
# EXPORT scope. Previously `resolve_code_var` always returned a by-name GLOBAL
# routine ref for operators, which dangled once EXPORT returned and re-dispatched
# the operator by name forever (infinite recursion / stack overflow). (Understitch)

use lib $?FILE.IO.parent.add('lib').Str;

use ExportInfix;   # installs infix:<jn> that joins with "-"

is "a" jn "b", "a-b", 'EXPORT-installed infix operator works';
is &infix:<jn>("x", "y"), "x-y", 'explicit &infix:<jn> call works';
is ("p" jn "q" jn "r"), "p-q-r", 'chained EXPORT-installed operator works';

my &op = &infix:<jn>;
is op("m", "n"), "m-n", 'operator ref bound to a lexical is callable';

# Closure capture: the operator body reads the EXPORT parameter.
is (1 jn 2), "1-2", 'operator captures the EXPORT separator';

# A plain top-level operator still resolves and stays callable.
sub infix:<box>($a, $b) { "[$a,$b]" }
is &infix:<box>(3, 4), "[3,4]", 'top-level operator ref is callable';
is (5 box 6), "[5,6]", 'top-level operator applies by syntax';
