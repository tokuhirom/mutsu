use Test;

plan 6;

class T {
    method a { 'test' }
    method b($x) { $x }
    method c($x, $y) { $x + $y }
}

my $o = T.new;
is (a $o:), 'test', 'indirect object notation without args and without parens';
is (b $o: 42), 42, 'indirect object notation with one arg and without parens';
is (c $o: 20, 22), 42, 'indirect object notation with multiple args and without parens';
is a($o:), 'test', 'indirect object notation with parens and no args';
is EVAL('abs -42:'), 42, 'indirect object notation with colon at EOF';
my $meth = 'a';
is $o.$meth, 'test', 'dynamic method call with variable method name and no parens';
