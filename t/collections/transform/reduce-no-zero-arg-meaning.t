use Test;

plan 16;

my $repeat = [x] ();
isa-ok $repeat, Failure, '[x] over an empty list returns a Failure';
isa-ok $repeat.exception, X::NoZeroArgMeaning,
    'the Failure wraps X::NoZeroArgMeaning';
is $repeat.exception.name, 'infix:<x>', 'the exception names infix:<x>';

my $repeat-callable = ().reduce(&infix:<x>);
isa-ok $repeat-callable, Failure,
    '.reduce with &infix:<x> uses the same zero-argument classification';
is $repeat-callable.exception.name, 'infix:<x>',
    'the callable spelling preserves the operator name';

isa-ok ([xx] ()), Failure, 'xx has no zero-argument meaning';
isa-ok ([/] ()), Failure, '/ has no zero-argument meaning';
isa-ok ([%] ()), Failure, '% has no zero-argument meaning';
isa-ok ([%%] ()), Failure, '%% has no zero-argument meaning';
isa-ok ([+<] ()), Failure, '+< has no zero-argument meaning';
isa-ok ([~&] ()), Failure, '~& has no zero-argument meaning';
isa-ok ([gcd] ()), Failure, 'gcd has no zero-argument meaning';

is ([+] ()), 0, '+ keeps its zero-argument identity';
is ([~] ()), '', '~ keeps its zero-argument identity';
is ([minmax] ()).raku, 'Inf..-Inf', 'minmax keeps its range identity';
is ([\x] ()).elems, 0, 'triangle reduction over an empty list stays empty';
