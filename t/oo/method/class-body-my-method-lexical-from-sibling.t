use Test;

# A `my method` of a class body is lexical to the body: sibling methods call it
# by name and take it as `&name` (annotations' Binder passes `:&FETCH, :&STORE`
# to Proxy.new, #15259).

plan 5;

class Lex {
    my method secret { 'secret' }
    method call-it { secret(self) }
    method as-code { &secret }
    method as-sub-call { self.&secret }
}

is Lex.new.call-it, 'secret', 'called by name from a sibling method';
ok Lex.as-code.defined, '&name is defined in a sibling method';
is Lex.as-code.name, 'secret', 'and is the method';
is Lex.new.as-sub-call, 'secret', '.&name works from a sibling method';
nok Lex.can('secret'), 'it is not in the method table';
