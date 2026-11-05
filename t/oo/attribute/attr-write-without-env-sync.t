use Test;

# A method that writes its attributes (the non-pure frame shape) leaves them
# visible to the caller and to later calls without the frame copying its
# attribute locals back into the env first (ADR-14633).

plan 9;

class Counter {
    has $!n = 0;
    has @!log;
    has %!seen;
    has $.pub = 'p';
    method bump($by) { $!n += $by; @!log.push($by); %!seen{$by}++; $!n }
    method total { $!n }
    method log { @!log.join(',') }
    method seen { %!seen.keys.sort.join(',') }
    method set-pub($v) { $!pub = $v; my &c = { $!pub ~ '!' }; c() }
}

my $c = Counter.new;
is $c.bump(2), 2, 'a scalar attribute accumulates';
is $c.bump(3), 5, 'and keeps accumulating across calls';
is $c.total, 5, 'a later method sees the writes';
is $c.log, '2,3', 'an array attribute grows';
is $c.seen, '2,3', 'a hash attribute grows';
is $c.set-pub('q'), 'q!', 'a public attribute written in a method reaches its closure';
is $c.pub, 'q', 'and the caller';

# A `:=` bound attribute aliases the outer variable past the method's exit.
class Binder {
    has $!x;
    method bind($outer is rw) { $!x := $outer; 1 }
    method set($v) { $!x = $v }
}
my $outer = 1;
my $b = Binder.new;
$b.bind($outer);
$b.set(7);
is $outer, 7, 'a := bound attribute writes through to the outer variable';
is $b.set(8), 8, 'and keeps doing so on the next call';
