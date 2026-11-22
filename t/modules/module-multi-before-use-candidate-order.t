use v6;
use Test;
use lib 'roast/packages/Test-Helpers';
use Test::Util;

plan 7;

# rakudo installs a `multi` candidate when the parser reaches it, so one the
# program declares before `use M` ranks before the candidates M exports, and one
# declared after it ranks after them; the import extends the program's own
# family rather than replacing it. mutsu loads M while it parses the `use`
# (ADR-17558), so the program's candidates take their order from the parse too
# (slice 5b), and a `multi` before the `use` no longer holds the load back.
my $dir = make-temp-dir;
$dir.add("PoBare.rakumod").spurt: q:to/END/;
    multi trait_mod:<is>(Routine $r, :$po-tagged!) is export { say "module trait on {$r.name}" }
    multi po-greet(Int $x) is export { say "module greet Int" }
    say "PoBare loaded";
    END
$dir.add("PoUnit.rakumod").spurt: q:to/END/;
    unit module PoUnit;
    multi po-greet(Int $x) is export { say "module greet Int" }
    END

sub out-of(Str $code) {
    # stderr is left out: rakudo also warns about the redeclared candidate.
    my $proc = run $*EXECUTABLE, '-I', $dir.Str, '-e', $code, :out, :err;
    $proc.err.slurp(:close);
    $proc.out.slurp(:close)
}

my $code;

$code = q:to/END/;
    multi trait_mod:<is>(Routine $r, :$po-tagged!) { say "unit trait on {$r.name}" }
    use PoBare;
    sub f() is po-tagged { }
    END
is out-of($code), "PoBare loaded\nunit trait on f\n",
    'a trait candidate declared before the use wins the tie';

$code = q:to/END/;
    use PoBare;
    multi trait_mod:<is>(Routine $r, :$po-tagged!) { say "unit trait on {$r.name}" }
    sub f() is po-tagged { }
    END
is out-of($code), "PoBare loaded\nmodule trait on f\n",
    'a trait candidate declared after the use loses the tie';

$code = q:to/END/;
    multi po-greet(Str $x) { say "unit greet Str" }
    use PoBare;
    say 1 +;
    END
is out-of($code), "PoBare loaded\n",
    'the module loads while the use is parsed, ahead of a later compile error';

$code = q:to/END/;
    multi po-greet(Str $x) { say "unit greet Str" }
    use PoBare;
    po-greet(1); po-greet("a");
    say &po-greet.candidates.map(*.signature.raku);
    END
is out-of($code), "PoBare loaded\nmodule greet Int\nunit greet Str\n(:(Str \$x) :(Int \$x))\n",
    'a bare-file import extends the family the program declared before it';

$code = q:to/END/;
    multi po-greet(Str $x) { say "unit greet Str" }
    use PoUnit;
    po-greet(1); po-greet("a");
    say &po-greet.candidates.map(*.signature.raku);
    END
is out-of($code), "module greet Int\nunit greet Str\n(:(Str \$x) :(Int \$x))\n",
    'a unit-module import extends the family the program declared before it';

$code = q:to/END/;
    use PoUnit;
    multi po-greet(Str $x) { say "unit greet Str" }
    say &po-greet.candidates.map(*.signature.raku);
    END
is out-of($code), "(:(Int \$x) :(Str \$x))\n",
    'a candidate declared after the use is listed after the imported ones';

$code = q:to/END/;
    multi po-greet(Int $x) { say "unit greet Int" }
    use PoUnit;
    po-greet(1);
    CATCH { default { say .^name } }
    END
is out-of($code), "X::Multi::Ambiguous\n",
    'equal candidates of the program and the module stay ambiguous';
