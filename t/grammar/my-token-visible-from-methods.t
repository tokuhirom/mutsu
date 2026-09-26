use Test;

plan 5;

# A `my token`/`my rule` declared in a package body is lexically visible to
# every routine written inside that body, methods included, and to the
# methods of a class nested in it. Getopt::Long validates option names with
#
#     my rule name { [\w+]+ % '-' | '?' }
#     class Option { submethod TWEAK(:@names) { die ... if any(@names) !~~ &name } }
#
# and `&name` read back as Nil inside the submethod, so every option name was
# "invalid".

class P {
    my token word { \w+ }
    method direct($s) { so $s ~~ &word }
    class Inner {
        method nested($s) { so $s ~~ &word }
    }
    method via-inner($s) { Inner.nested($s) }
}

ok P.direct('abc'), 'a method sees its class body\'s my token';
nok P.direct('!!'), '... and it is the right token';
ok P.via-inner('abc'), 'a nested class\'s method sees the outer body\'s my token';

class Q {
    my rule name { [\w+]+ % '-' | '?' }
    class Option {
        has @.names;
        submethod TWEAK(:@names) {
            die "Invalid name(s): @names[]" if any(@names) !~~ &name;
        }
    }
    method make(*@n) { Option.new(:names(@n)) }
}

lives-ok { Q.make(<regex r start-service ?>) }, 'Getopt::Long-shaped name check accepts valid names';
dies-ok { Q.make('!!') }, '... and still rejects an invalid one';
