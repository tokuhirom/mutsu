use lib 't/lib';
use Test;

# A slang action that reads a declared method's `meta-object` twice gets the
# same object both times, so a role mixed into the first answer is there for
# the second (#17415; rakudo memoizes the meta-object on the node). Checked
# against rakudo (RakuAST frontend).

plan 3;

use MetaObjectIdentitySlang;

class Greeter {
    tagged hello { 'hi' }
}

my $hello = Greeter.^find_method('hello');
ok $hello.can('tag'), 'the role mixed into the first meta-object answer';
is $hello.tag, 'same-object', 'the attribute written through the second';
is Greeter.hello, 'hi', 'the method runs';
