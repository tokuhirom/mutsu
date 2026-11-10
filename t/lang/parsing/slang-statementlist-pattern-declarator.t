use lib 't/lib';
use Test;

# A slang `routine-declarator` that reads a method with the core `method-def`
# and parses its body in the slang's own language (#16888, EEL 0.3's `pattern`):
# the grammar's `statementlist` sees `$*BLOCK` as the method being declared, its
# name and signature; the action's `$method.meta-object` is the method the class
# installs; `RakuAST::Literal.from-value` carries an object into the body; and the
# `&make` the slang's `EXPORT` hands the importer stays the importer's.

plan 9;

use PatternDeclaratorSlang;

class Greeter {
    shout hello(Int $x, $y) { hi }
    method plain { 42 }
    shout bye { there }
}

is Greeter.hello(1, 2), 'HI!', 'the body is the node the slang attached';
is Greeter.bye, 'THERE!', 'a second declaration';
is Greeter.plain, 42, 'an ordinary method keeps the core statement list';

my $hello = Greeter.^find_method('hello');
ok $hello.can('tag'), 'the action mixed a role into the class method';
is $hello.tag, 'hello/2/hi', '$*BLOCK is the method: its name, signature and body word';
is Greeter.^find_method('bye').tag, 'bye/none/there', 'a method without a signature';
nok Greeter.^find_method('plain').can('tag'), 'an ordinary method is left alone';

is make(5), 'the importer make', "the importer gets EXPORT's make";

{
    my $sub = sub { shout-free() };
    sub shout-free { 'no slang here' }
    is $sub(), 'no slang here', 'a sub body is parsed as usual';
}
