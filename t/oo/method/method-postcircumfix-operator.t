use Test;

plan 2;

class A {
    has %!attrs;
    method postcircumfix:<{ }>($key) { %!attrs{$key} }
}

eval-lives-ok q[
class A2 {
    has %!attrs;
    method postcircumfix:<{ }>($key) { %!attrs{$key} }
}
A2.new(:attrs({ foo => "bar" }))<foo>;
], "postcircumfix method name parses in class and can be invoked with <...>";

eval-lives-ok q[
class B {
method postcircumfix:<{ }>($table) {
}
}

{
1;
}
], "postcircumfix method parses with odd whitespace and no extra semicolon";
