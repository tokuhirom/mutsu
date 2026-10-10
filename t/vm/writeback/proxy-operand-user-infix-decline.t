use Test;

plan 3;

# A user `multi infix:<~>` with `is rw` parameters sees the Proxy raw. When no
# user candidate matches the call, the native `~` must still FETCH the Proxy
# operand instead of stringifying the Proxy object ("Proxy"). (#12562)
class AST { }
multi infix:<~>(AST $a, AST $b) { "ast" }
multi infix:<~>(Str() $a is rw, AST $b) { "rw-ast" }

class Foo {
    has $.t = "abc";
    method title is rw { Proxy.new(FETCH => -> $ { $!t }, STORE => -> $, $v { $!t = $v }) }
}
my $f = Foo.new;

is $f.title ~ "x", 'abcx', 'rw-method Proxy on the left of a declined user ~';
is "x" ~ $f.title, 'xabc', 'rw-method Proxy on the right of a declined user ~';
my $blk = -> $_ { (.title ~ "\n") x 2 };
is $blk($f), "abc\nabc\n", 'topic method Proxy in a repeated ~ operand';
