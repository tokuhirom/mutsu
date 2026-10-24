use v6;
use Test;
use lib 'roast/packages/Test-Helpers';
use Test::Util;

plan 4;

# `CALLERS::<&name>` from a routine called by a method of a role WITH type
# parameters finds a `sub` the role's file declares at its top level.
my $dir = make-temp-dir;
$dir.add("CsParamProbe.rakumod").spurt: q:to/END/;
    unit class CsParamProbe;
    method check(\SELF:) { ?(::CALLERS::<&cs-param-marker>) ~ " " ~ ?(::CALLERS::OUTERS::<&cs-param-marker>) }
    END
$dir.add("CsParamHolder.rakumod").spurt: q:to/END/;
    use CsParamProbe;
    unit role CsParamHolder[$of, Str $k];
    sub cs-param-marker { True }
    method pub { CsParamProbe.check }
    END
$dir.add("CsParamBlock.rakumod").spurt: q:to/END/;
    use CsParamProbe;
    role CsParamBlock[::T] {
        sub cs-param-marker { True }
        method pub { CsParamProbe.check }
    }
    END

sub out-of(Str $code) {
    my $proc = run $*EXECUTABLE, '-I', $dir.Str, '-e', $code, :out, :err;
    $proc.out.slurp(:close) ~ $proc.err.slurp(:close)
}

is out-of('use CsParamHolder; class X does CsParamHolder[Int, "k"] { }; say X.new.pub'), "True True\n", "a class composing the parameterized role";
is out-of('use CsParamHolder; say CsParamHolder[Int, "k"].new.pub'), "True True\n", "the role pun itself";
is out-of('use CsParamBlock; class X does CsParamBlock[Int] { }; say X.new.pub'), "True True\n", "a role block with a type capture";
is out-of('use CsParamProbe; say CsParamProbe.check'), "False False\n", "the program does not declare it, so no caller does";
