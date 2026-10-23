use v6;
use Test;

plan 8;

class A { method hi { "hi" } }

my method lexical { "lexical" }

throws-like { A.lexical }, X::Method::NotFound,
    "a lexical my method is not a method of a class";
throws-like { A.new.lexical }, X::Method::NotFound,
    "a lexical my method is not a method of an instance";
throws-like { 5.lexical }, X::Method::NotFound,
    "a lexical my method is not a method of Int";
is A.?lexical.raku, "Nil", ".? on a lexical my method name is Nil";
is A.&lexical, "lexical", ".& still calls the lexical method";
is lexical(A), "lexical", "the lexical method is callable by name";

# A type minted with the name of a declared class gets its own methods.
my $t := Metamodel::ClassHOW.new_type(:name<A>);
$t.^add_method: "no-table", my method no-table { True };
$t.^compose;
is $t.no-table, True, "the minted type has the method";
throws-like { A.no-table }, X::Method::NotFound,
    "the declared class of the same name does not";
