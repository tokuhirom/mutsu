use Test;
plan 1;

grammar G {
    regex name       { <-restricted +name -sep>+ }
    token restricted { <[ : < > ( ) ]> }
    token name-sep   { '::' }
}

# A class naming the enclosing rule itself used to overflow the stack.
dies-ok { G.subparse('Foo::Bar', :rule<name>) }, 'self-referential class with undeclared rule dies catchably';
