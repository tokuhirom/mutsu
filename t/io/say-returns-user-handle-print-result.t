# With `$*OUT` replaced by an object, `say`/`put`/`print` answer what that object's `print` answered
# (Rakudo's `say` is `$*OUT.print(...)` in tail position), so Jupyter's `True but role { method
# __hide }` marker survives (#16585).
use Test;
plan 8;

class P {
    has @.got;
    method print(*@a) { @!got.push: @a.join; "from-print" }
    method flush { }
}

sub run(&code) {
    my $p = P.new;
    my $*OUT = $p;
    my $r = code();
    ($r, $p.got)
}

my ($r, $got) = run({ say "a" });
is $r, 'from-print', 'a block ending in `say` yields the handle\'s answer';
is-deeply $got, ["a\n"], 'and the text reached the handle';

($r, $got) = run({ put "b" });
is $r, 'from-print', 'put';

($r, $got) = run({ print "c" });
is $r, 'from-print', 'print';

sub tail-say { say "d" }
my $p = P.new;
{
    my $*OUT = $p;
    $r = tail-say();
}
is $r, 'from-print', 'a routine whose last statement is `say`';

class Hidden {
    method print(*@a) { True but role { method __hide { True } } }
    method flush { }
}
{
    my $*OUT = Hidden.new;
    $r = do { say "x" };
}
ok $r.?__hide, 'a role mixed into the answer survives';

$r = do { say "to the real handle" };
is $r, True, 'with a real handle the value is still True';

$r = do { print "" };
is $r, True, 'print too';
