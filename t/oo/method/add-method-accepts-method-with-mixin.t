use Test;

# From AttrX::Mooish: `anon method () {...} but Tag[$x]` handed to ^add_method.

plan 3;

role Tag[$n] { method tag { $n } }

my class H { }
my $m = anon method () { "hi" } but Tag['x'];
H.^add_method('hello', $m);
H.^compose;
is H.new.hello, 'hi', 'a method with a mixin is installed';

my class K {
    method setup($name --> Nil) {
        my sub helper(Mu \obj, $a) is raw { "obj-$a" }
        K.^add_method('hi2', anon method () { helper(self, $name) } but Tag[$name]);
    }
}
K.setup('q');
K.^compose;
is K.new.hi2, 'obj-q', 'it keeps the lexical sub of the declaring method';
ok K.^can('hi2'), 'and is found by name';
