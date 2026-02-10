use Test;

plan 14;

# Basic $!attr mutation
class Counter {
    has $.count = 0;
    method increment() {
        $!count = $!count + 1;
    }
    method get-count() {
        return $!count;
    }
}

my $c = Counter.new();
is $c.count, 0, 'initial count is 0';
$c.increment();
is $c.count, 1, 'count after first increment';
$c.increment();
is $c.count, 2, 'count after second increment';

# $!attr mutation with compound assignment
class Accumulator {
    has $.total = 0;
    method add($n) {
        $!total = $!total + $n;
    }
}

my $a = Accumulator.new();
$a.add(10);
is $a.total, 10, '$!attr mutation with addition';
$a.add(5);
is $a.total, 15, '$!attr mutation accumulates';

# Multiple attribute mutation
class Rect {
    has $.width = 0;
    has $.height = 0;
    method scale($factor) {
        $!width = $!width * $factor;
        $!height = $!height * $factor;
    }
    method area() {
        return $!width * $!height;
    }
}

my $r = Rect.new(width => 3, height => 4);
is $r.area(), 12, 'area before scaling';
$r.scale(2);
is $r.width, 6, 'width after scale';
is $r.height, 8, 'height after scale';
is $r.area(), 48, 'area after scaling';

# Private attribute mutation
class SecretBox {
    has $!secret;
    has $.label;
    method set-secret($val) {
        $!secret = $val;
    }
    method get-secret() {
        return $!secret;
    }
}

my $box = SecretBox.new(secret => 'old', label => 'box1');
is $box.get-secret(), 'old', 'initial private attr value';
$box.set-secret('new');
is $box.get-secret(), 'new', 'private attr mutated via method';

# String attribute mutation
class Greeter {
    has $.greeting = 'Hello';
    method set-greeting($g) {
        $!greeting = $g;
    }
}

my $g = Greeter.new();
is $g.greeting, 'Hello', 'default greeting';
$g.set-greeting('Hi');
is $g.greeting, 'Hi', 'greeting mutated';

# Chained mutations within single method
class Account {
    has $.balance = 0;
    method deposit-and-double($amount) {
        $!balance = $!balance + $amount;
        $!balance = $!balance * 2;
    }
}

my $acc = Account.new(balance => 100);
$acc.deposit-and-double(50);
is $acc.balance, 300, 'multiple mutations in single method';

done-testing;
