unit module RoleLexicalScopeTypes;

class Handle is export { has Int $.n = 1 }

enum Flavour is export ( Sweet => 'sweet', Sour => 'sour' );

constant MAGIC is export = 42;

sub helper() is export { 'helped' }
