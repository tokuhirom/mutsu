use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

# A program's final statement is in sink context like every other statement
# (#9766): an unhandled Failure there throws, a failed `shell` dies and a user
# `sink` method runs. An EVAL's final statement is its value instead, and is
# not sunk. Measured against rakudo.

plan 9;

is_run 'sub foo { fail "boom" }; foo()',
    { out => '', err => /boom/, status => 1 },
    'a sub call returning a Failure as the last statement throws';

is_run 'shell "exit 1"',
    { out => '', err => /'exited unsuccessfully'/, status => 1 },
    'a failed shell as the only statement dies';

is_run 'class S { method sink { say "sunk" } }; say "-"; S.new',
    { out => "-\nsunk\n", err => '', status => 0 },
    'a fresh instance as the last statement has its sink method called';

is_run 'my @a; @a.pop',
    { out => '', err => /'empty Array'/, status => 1 },
    'a method call returning a Failure as the last statement throws';

is_run 'say "a"; Failure.new("x").so',
    { out => "a\n", err => '', status => 0 },
    'a handled Failure as the last statement does not throw';

is_run 'my $f = Failure.new("soft"); say "ok"; $f',
    { out => "ok\n", err => /'Useless use'/, status => 0 },
    'a bare variable holding a Failure as the last statement is not sunk';

is_run 'my $r = EVAL q[sub foo { fail "boom" }; foo()]; say $r.^name; $r.so',
    { out => "Failure\n", err => '', status => 0 },
    'an EVAL\'s final statement is its value, not sunk';

is_run 'sub f { 42 }; f()',
    { out => '', err => '', status => 0 },
    'an ordinary last-statement value is sunk silently';

is_run 'my $x = 0; sub f { (1..3).map({ $x += $_ }) }; f(); END say $x',
    { out => "6\n", err => '', status => 0 },
    'a sunk lazy map as the last statement runs its callback';
