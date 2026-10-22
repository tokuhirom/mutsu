use Test;
use nqp;

plan 10;

class P {
    has Mu $.val;
    method bind-arr  { $!val := Array[Str].new("a") }
    method assign-arr { $!val = Array[Str].new("a") }
    method bind-hash { $!val := Hash[Int].new }
    method bind-int  { $!val := 5 }
    method assign-int { $!val = 5 }
    method var-name { $!val.VAR.WHAT.^name }
    method cont { nqp::iscont($!val) }
}

my $o = P.new; $o.bind-arr;
is $o.var-name, 'Array[Str]', 'bound Array: .VAR is the Array, from another method';
is $o.cont, 0, 'bound Array: not a container';
$o = P.new; $o.assign-arr;
is $o.var-name, 'Scalar', 'assigned Array: .VAR is a Scalar';
is $o.cont, 1, 'assigned Array: a container';
$o = P.new; $o.bind-hash;
is $o.var-name, 'Hash[Int]', 'bound Hash: .VAR is the Hash';
is $o.cont, 0, 'bound Hash: not a container';
$o = P.new; $o.bind-int;
is $o.var-name, 'Int', 'bound Int: .VAR is the value';
$o = P.new; $o.assign-int;
is $o.var-name, 'Scalar', 'assigned Int: .VAR is a Scalar';

class Q { has $.a = [1, 2]; method n { $!a.VAR.^name } }
is Q.new.n, 'Scalar', 'defaulted aggregate attribute is a Scalar';
is Q.new(a => [3]).n, 'Scalar', 'constructor-supplied aggregate is a Scalar';
