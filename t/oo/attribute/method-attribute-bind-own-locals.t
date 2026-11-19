use Test;

# A method whose `:=` binds are all to its own `my` locals and parameters
# leaves the invocant's attributes alone, while an attribute bind
# (`$!x := ...`) in the same method still takes effect. The exit-time scan
# for attribute rebinds now skips readings it would reject for every
# attribute before looking at the attributes (#17335).

plan 9;

class C {
    has $.val;
    has $.instance;
    has $!lock;
    method locals-only(Mu $instance is raw) {
        my $val := $instance;
        my Mu $lock := 42;
        $val
    }
    method bind-attr(Mu $v is raw) {
        my $local := $v;
        $!val := $local;
        self
    }
    method lock-value { $!lock }
}

my $c = C.new(val => 1, instance => 'i');
is $c.locals-only(7), 7, 'a method binding only its own locals returns';
is $c.val, 1, '...and leaves $!val alone';
is $c.instance, 'i', '...and $!instance';
nok $c.lock-value.defined, '...and $!lock';

my $x = 10;
$c.bind-attr($x);
is $c.val, 10, 'an attribute bind takes the bound value';
$x = 11;
is $c.val, 11, 'and stays bound to the caller variable';

my $d = C.new(val => 'd');
$d.locals-only($d);
is $d.val, 'd', 'a local bound to the invocant leaves its attributes alone';
$d.bind-attr(5);
is $d.val, 5, 'binding a value works too';
is $c.val, 11, 'the other instance is untouched';
