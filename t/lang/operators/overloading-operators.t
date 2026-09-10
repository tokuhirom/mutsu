use Test;

plan 7;

{
    sub postfix:<§> ($x) {
        $x * 2;
    }
    is 3§, 6, 'unicode postfix operator works';
}

{
    sub postfix:<!>($arg) {
        if ($arg == 0) { 1 }
        else { ($arg - 1)! * $arg }
    }
    is 5!, 120, 'recursive postfix operator works';
    is 5.!, 120, 'dotted postfix operator works';
}

{
    my @keys;
    class A does Associative {
        multi method AT-KEY(A:D: $key) {
            push @keys, $key;
            ++state $i
        }
    }

    is A.new<foo bar>, (1, 2), 'AT-KEY indexing works on instances';
    is @keys, [<foo bar>], 'AT-KEY called once per key';
}

{
    class B {
        has $.x;
        method CALL-ME($y) {
            $.x ~ $y;
        }
    }
    is B.new(x => 'a').('b'), 'ab', 'CALL-ME invocation overloading works';
}

$ = "";
sub postfix:<♥> ($) { "pass" }
is "{ 5♥ }", 'pass', 'interpolation sees newly defined postfix operators';
