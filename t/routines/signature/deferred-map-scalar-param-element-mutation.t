use Test;

plan 6;

# A deferred `.map` the CALLER sinks runs its block after the sub's frame is
# gone. A scalar parameter whose only use in the block is an element delete or
# increment (`$x{4}:delete`, `$x{4}++`) must still be captured, or the block
# mutates a stale copy (App::RaCoCo's DataPart.plus).
sub del($x) { (1,).map({ $x{4}:delete }) }
my $a = {4 => 1, 5 => 2};
del($a);
is-deeply $a, {5 => 2}, ':delete on a scalar param in a returned map';

sub del-expr($x) { (1,).map({ $x<4>:delete; 1 }) }
my $b = {4 => 1, 5 => 2};
del-expr($b);
is-deeply $b, {5 => 2}, ':delete with the angle-bracket subscript';

sub inc($x) { (1,).map({ $x{4}++ }) }
my $c = {4 => 1};
inc($c);
is-deeply $c, {4 => 2}, 'postfix ++ on an element';

sub dec($x) { (1,).map({ --$x{4} }) }
my $d = {4 => 1};
dec($d);
is-deeply $d, {4 => 0}, 'prefix -- on an element';

sub arr($x) { (1,).map({ $x[1]:delete }) }
my $e = [1, 2, 3];
arr($e);
is $e[1].defined, False, ':delete on an element of an array param';

# The same shapes inside the frame keep working.
sub inside($x) { (1,).map({ $x{4}:delete }).sink; 1 }
my $f = {4 => 1};
inside($f);
is-deeply $f, {}, 'sunk inside the sub';
