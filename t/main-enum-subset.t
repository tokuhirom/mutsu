use Test;
use lib $*PROGRAM.parent(1).add("roast/packages/Test-Helpers");
use Test::Util;

plan 8;

# MAIN with an enum-typed positional/named parameter coerces CLI strings.
my $enum-code = Q:to/END/;
    enum Hand <Rock Paper Scissors>;
    sub MAIN (Hand $hand, Hand :$pos-hand) {
        print "pass";
    }
    END

is_run $enum-code, :args[<Rock>], { :out<pass>, :err('') },
    'enum positional coerces';
is_run $enum-code, :args[<--pos-hand=Scissors Rock>], { :out<pass>, :err('') },
    'enum positional + named coerces';
is_run $enum-code, :args[<Hand>], { :out{ not .contains: 'pass' }, :err(/'=<Hand>'/) },
    'enum name itself is rejected and usage shows the enum';

# MAIN with a subset-typed parameter keeps the string and validates via `where`.
my $subset-code = Q:to/END/;
    subset S where / 'ok' /;
    sub MAIN (S $s-pos, S :$s-named = "ok") {
        print "pass";
    }
    END

is_run $subset-code, :args[<ok>], { :out<pass>, :err('') },
    'subset positional works';
is_run $subset-code, :args[<--s-named=ok ok>], { :out<pass>, :err('') },
    'subset positional + named works';
is_run $subset-code, :args[<S>], { :out{ not .contains: 'pass' }, :err(/'[=S]'/) },
    'subset name itself is rejected and usage shows the subset';

# Untyped slurpy MAIN args auto-convert to known enum values.
is_run 'sub MAIN(*@a) { say .raku for @a }',
    :args<True False Less More BigEndian>,
    { :out("Bool::True\nBool::False\nOrder::Less\nOrder::More\nEndian::BigEndian\n"), :err('') },
    'untyped slurpy args auto-convert to enums';

# %*SUB-MAIN-OPTS<named-anywhere> allows named args interspersed with positionals.
is_run ｢
    my %*SUB-MAIN-OPTS = :named-anywhere;
    sub MAIN ($a, $b, :$c, :$d) { print "$a$b$c$d" }
｣, :args[<1 --c=2 3 --d=4>], { :out<1324>, :err(''), :0status },
    'named-anywhere allows interspersed named args';
