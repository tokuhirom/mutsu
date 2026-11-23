use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

plan 8;

# `Failure.new` records the backtrace of its creation, without the frame that
# called it, as rakudo does (#18987).
sub mk { Failure.new("y") }
my $g = mk();
$g.so;
is $g.backtrace[0].subname, '<unit>', 'the caller of the creating routine comes first';

sub b { Failure.new("z") }
sub a { b() }
my $h = a();
$h.so;
is $h.backtrace.list.map(*.subname).join(','), 'a,<unit>', 'every outer frame is kept';

my $top = Failure.new("x");
$top.so;
is $top.backtrace.list.elems, 0, 'a top-level Failure.new has no frames';

sub d { die "x"; CATCH { default { return Failure.new($_) } } }
my $d = d();
$d.so;
is $d.backtrace.list.map(*.subname).grep(* eq 'd').elems, 1,
    'a thrown exception keeps its own backtrace';

# An uncaught top-level Failure.new names only the throw site.
is_run 'my $f = Failure.new("x"); sub q { $f.sink }; q()',
    { :out(''), :err(/^ "x\n\nActually thrown at:\n  in sub q at " \N+ "\n  in block <unit> at " \N+ \n+ $/) },
    'an uncaught top-level Failure.new shows where it was thrown';
is_run 'sub mk { Failure.new("y") }; sub q { mk().sink }; q()',
    { :out(''), :err(/^ "y\n  in sub q at " \N+ "\n  in block <unit> at " \N+ \n+ $/) },
    'the creation site matches the throw site';
is_run 'sub a { b() }; sub b { Failure.new("z") }; sub q { a().sink }; q()',
    { :out(''), :err(/^ "z\n  in sub a at " \N+ "\n  in sub q at " \N+ "\n  in block <unit> at " \N+ \n+ $/) },
    'the creation site is shown, not the throw site';

# `fail` keeps its own frame.
sub ff { fail "f" }
my $k = ff();
$k.so;
is $k.backtrace.list.map(*.subname).join(','), 'ff,<unit>', 'fail records the failing routine';
