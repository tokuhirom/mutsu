use Test;

# A Proxy given as a named constructor argument is FETCHed as it lands in the
# attribute's Scalar, on both the native default-constructor path and the
# interpreter's (a class composing a role). Found via Red::AST::Value
# (RedX::HashedPassword).
plan 5;

my $store = "abc";
my $x := Proxy.new(FETCH => method { $store }, STORE => method (\v) { $store = v });
role R { }
class A1 is Any { has $.value; method gv { $!value } }
class A2 does R { has $.value; method gv { $!value } }
class A3 { has $.value; has Str $.t; method gv { $!value } }

for A1, A2, A3 -> \T {
    $store = "abc";
    my $o = T.new(:value($x));
    $store = "zzz";
    is $o.gv, "abc", T.^name ~ ': value FETCHed at construction';
}
my $n = A3.new(:value($x), :t($x));
is $n.t, "zzz", 'typed attribute accepts a Proxy over a Str';
$store = "q";
is $n.value, "zzz", 'and keeps the fetched value';
