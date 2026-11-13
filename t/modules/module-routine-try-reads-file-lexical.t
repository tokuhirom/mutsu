use Test;
use lib 't/lib';
use TryFileLexical;

# A module routine called from the importing unit reads the module's own
# file-scope lexicals inside a `try` as it does outside one (#17732; Dawa's
# `maybe-stop` locks a module `my Lock` inside `try`).

plan 3;

is try-block(), 5, 'try block reads the module lexical';
is try-statement(), 10, 'statement-form try reads the module lexical';
is try-lock(), 6, 'a module Lock is usable inside try';
