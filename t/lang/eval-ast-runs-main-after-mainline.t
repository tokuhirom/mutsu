use Test;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;

# EVAL of the RakuAST of a whole program that declares `sub MAIN` dispatches
# MAIN after the mainline, inside the unit; a string EVAL does not.

plan 4;

my @log;

my $main-src = q:to/END/;
@*ARGS = <a b>;
@log.push("mainline");
sub MAIN($x, $y) { @log.push("MAIN $x $y") }
END

EVAL($main-src.AST);
is @log.join(','), 'mainline,MAIN a b', 'EVAL($ast) runs MAIN after the mainline';

@log = ();
EVAL($main-src);
is @log.join(','), 'mainline', 'a string EVAL does not run MAIN';

my $semi = q:to/END/;
@*ARGS = <p q r>;
sub MAIN($a, $b, *@rest);
@log.push("$a $b @rest[]");
END
@log = ();
EVAL($semi.AST);
is @log.join(','), 'p q r', 'semicolon MAIN: the rest of the unit is its body';

@log = ();
EVAL(q:to/END/.AST);
@log.push("no main");
END
is @log.join(','), 'no main', 'a unit without MAIN runs as before';
