use Test;

plan 9;

role Lowerable {
    multi method lc($string:) { "role-lc" }
    multi method substr(|c) { "role-substr" }
}
class Col does Lowerable {
    method Str { "Col<1>" }
    method uc { "UC" }
    method flip { "FLIP" }
    method chomp { "CH" }
    method ord { 7 }
}
my $c = Col.new;

is lc($c), 'role-lc', 'lc(obj) calls the composed role method';
is (lc $c), 'role-lc', 'the prefix spelling does too';
is uc($c), 'UC', 'uc(obj) calls the class method';
is flip($c), 'FLIP', 'flip(obj) calls the class method';
is chomp($c), 'CH', 'chomp(obj) calls the class method';
is ord($c), 7, 'ord(obj) calls the class method';
is substr($c, 0, 2), 'role-substr', 'substr(obj, ...) passes its remaining arguments to the method';
my &blk = { lc .self };
is blk($c), 'role-lc', 'a routine call inside a block forwards too';

is uc("abc"), 'ABC', 'a Str is unaffected';
