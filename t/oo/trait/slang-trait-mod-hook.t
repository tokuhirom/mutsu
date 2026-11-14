use v6;
use lib 't/lib';
use Test;

# A slang may add a `trait_mod` candidate (OO::Plugin adds `for`, `after`,
# `before` and `demand`, #13370). It runs where a declaration's traits are
# parsed, ahead of the built-in ones: its guard's panic is the compile error
# at that trait, and every ordinary trait is untouched.

use TraitModSlang;

plan 5;

class Plain is Any does Callable { method m() is rw { my $x = 1; $x } }
ok Plain.new.m == 1, 'a class and method with ordinary traits compile after the slang';

sub plain($x) is pure returns Int { $x * 2 }
is plain(21), 42, 'so does a routine with ordinary traits';

grammar G is Grammar { token TOP { x } }
ok G.parse('x'), 'and a grammar';

my $source = q:to/END/;
    use TraitModSlang;
    class C tagged Foo { }
    say 'compiled';
    END

my $proc = run $*EXECUTABLE.absolute, '-I', 't/lib', '-e', $source, :out, :err;
my $err = $proc.err.slurp(:close);
$proc.out.slurp(:close);
isnt $proc.exitcode, 0, "the slang's trait is tried at a class's trait position";
like $err, /"Modificator 'tagged' is only applicable to a tagged declaration"/,
    "and its guard's panic is the compile error";

# vim: expandtab shiftwidth=4
