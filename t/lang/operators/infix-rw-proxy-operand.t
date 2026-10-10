use Test;

plan 3;

# An `is rw` method's Proxy result is a writable lvalue, so it binds a user
# infix candidate's `is rw` parameter (#12573).
class AST { }
class Foo {
    has $.t = "abc";
    method title is rw {
        Proxy.new(FETCH => -> $ { $!t }, STORE => -> $, $v { $!t = $v })
    }
}
my $f = Foo.new;

{
    multi infix:<~>(Str() $a is rw, AST $b) { "rw-ast" }
    is $f.title ~ AST.new, "rw-ast", "Proxy operand matches an `is rw` candidate";
}
{
    multi infix:<~>(Str() $a is rw, AST $b) { "rw-ast" }
    multi infix:<~>(Str() $a is readonly, AST $b) { "ro-ast" }
    is $f.title ~ AST.new, "rw-ast", "`is rw` candidate wins over `is readonly`";
}
is $f.title ~ "x", "abcx", "plain ~ unaffected";
