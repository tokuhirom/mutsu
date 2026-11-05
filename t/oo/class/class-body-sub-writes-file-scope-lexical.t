use Test;

# A `sub`, `my sub` or `my method` declared in a class body writes the
# file-scope lexical it names (#15782).

plan 4;

my $a = 0;
my $b = 0;
my $c = 0;
class C {
    sub plain { $a++ }
    my sub lexical { $b++ }
    my method meth { $c++ }
    method go { plain(); lexical(); meth(self) }
}
C.new.go;
C.new.go;

is $a, 2, 'a sub of the class body writes the outer scalar';
is $b, 2, 'a my sub of the class body writes it';
is $c, 2, 'a my method of the class body writes it';

my $log = '';
class D {
    my method record($x) { $log ~= $x }
    method add($x) { record(self, $x) }
}
D.new.add('x');
D.new.add('y');
is $log, 'xy', 'a my method appends to an outer string';
