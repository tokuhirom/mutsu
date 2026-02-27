use Test;

plan 2;

class Foo {
    method WHAT { 'Bar' }
}

my $obj = Foo.new;
my $meth = method () { 'Bar' };
is $obj.$meth, 'Bar', '.$meth dispatches method object with invocant';
is $obj.WHAT.gist, '(Foo)', '.WHAT keeps pseudo-method behavior';
