use v6;
use Test;
use lib 'roast/packages/Test-Helpers';
use Test::Util;

plan 4;

# `CALLERS::<&name>` from a routine called by a role's method finds a `sub` the
# role's file declares at its top level, even though the method runs with the
# composing class as its package.
my $dir = make-temp-dir;
$dir.add("CsRoleProbe.rakumod").spurt: q:to/END/;
    unit class CsRoleProbe;
    method check(\SELF:) { ?(::CALLERS::<&cs-marker>) ~ " " ~ ?(::CALLERS::OUTERS::<&cs-marker>) }
    END
$dir.add("CsRoleHolder.rakumod").spurt: q:to/END/;
    use CsRoleProbe;
    unit role CsRoleHolder;
    sub cs-marker { True }
    method pub { CsRoleProbe.check }
    method !priv { CsRoleProbe.check }
    method via-private { self!priv }
    END
$dir.add("CsClassHolder.rakumod").spurt: q:to/END/;
    use CsRoleProbe;
    unit class CsClassHolder;
    sub cs-marker { True }
    method pub { CsRoleProbe.check }
    END

sub out-of(Str $code) {
    my $proc = run $*EXECUTABLE, '-I', $dir.Str, '-e', $code, :out, :err;
    $proc.out.slurp(:close) ~ $proc.err.slurp(:close)
}

is out-of('use CsClassHolder; say CsClassHolder.new.pub'), "True True\n", "a class method sees its file's sub";
is out-of('use CsRoleHolder; class X does CsRoleHolder { }; say X.new.pub'), "True True\n", "a composed role method sees the role file's sub";
is out-of('use CsRoleHolder; class X does CsRoleHolder { }; say X.new.via-private'), "True True\n", "also through a private role method";
is out-of('use CsRoleProbe; say CsRoleProbe.check'), "False False\n", "the program does not declare it, so no caller does";
