use Test;

# A backtick is not a quoting construct in Raku: `cmd` is a compile error,
# never a shell command.

plan 2;

use MONKEY-SEE-NO-EVAL;
throws-like { EVAL q{`echo pwned`} }, X::Comp, 'backticks do not compile';
is qx{echo ok}.trim, 'ok', 'qx still runs a command';
