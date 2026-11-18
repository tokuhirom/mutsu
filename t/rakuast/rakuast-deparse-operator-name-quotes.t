use Test;
use experimental :rakuast;

plan 8;

# An operator routine's name keeps how its symbol was quoted (#18060).
sub dep($src) { $src.AST.DEPARSE.lines[0].trim }

is dep('sub postcircumfix:«<< >>»($a){1}'), 'sub postcircumfix:«<< >>» ($a) {', 'guillemet quote';
is dep('sub circumfix:«<< >>»($a){1}'), 'sub circumfix:«<< >>» ($a) {', 'guillemet circumfix';
is dep('sub postcircumfix:<< a b >>($a){1}'), 'sub postcircumfix:<< a b >> ($a) {', 'double-angle quote';
is dep('sub infix:«foo»($a,$b){1}'), 'sub infix:<<foo>> ($a, $b) {', 'guillemet infix';
is dep('sub infix:<foo>($a,$b){1}'), 'sub infix:<foo> ($a, $b) {', 'a plain angle quote is unchanged';

my $name = 'sub postcircumfix:<< a b >>($a){1}'.AST.find-nodes(RakuAST::Sub)[0].name;
is $name.colonpairs[0].processors.join(' '), 'quotewords val', 'a quotewords colonpair';
is $name.colonpairs[0].segments[0].value, ' a b ', 'with its written spaces';

is EVAL('sub infix:<< foo >>($a,$b){ 7 }; 3 foo 4'.AST), 7, 'EVAL of the AST declares the operator';
