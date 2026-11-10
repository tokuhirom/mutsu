use Test;

plan 4;

# A user HOW class's `find_method` override answers every method call on its
# instances and type object, with the call's arguments passed on.
my class MyHOW is Metamodel::ClassHOW {
    method find_method(Mu \obj, $name, | --> Mu) is raw {
        my $m = callsame;
        $name eq 'hello' | 'add'
          ?? -> \o, |c { "wrapped(" ~ $m(o, |c) ~ ")" }
          !! $m
    }
    method publish_method_cache(|) { }
}
my $k = MyHOW.new_type(:name<K>);
$k.^add_method('hello', method () { 'hi' });
$k.^add_method('add', method ($a, $b) { $a + $b });
$k.^add_method('plain', method () { 'plain' });
$k.^compose;

is $k.new.hello, 'wrapped(hi)', 'a wrapped method is found through the HOW';
is $k.new.add(2, 3), 'wrapped(5)', 'the call arguments reach the wrapper';
is $k.new.plain, 'plain', 'an unwrapped name still resolves';
is $k.^name, 'K', 'the type keeps its name';

# vim: expandtab shiftwidth=4
