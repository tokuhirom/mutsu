use Test;
use lib 't/lib';

# A type a `sub EXPORT` hook hands out under a literal key
# (`'MyAlias' => Int`, t/lib/UnitModuleExportSub.rakumod) is a type name to
# the importer: a signature in the importing block may name it (#14140).

plan 4;

{
    use UnitModuleExportSub;
    sub f(MyAlias $p) { $p }
    is f(3), 3, 'a parameter typed with the hook\'s type alias binds';
    my $s = 'x';
    dies-ok { f($s) }, 'and checks the alias\'s type';
    my MyAlias $x = 5;
    is $x, 5, 'a variable typed with it';
    is MyAlias.^name, 'Int', 'the alias is the type itself';
}
