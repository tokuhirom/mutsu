use Test;
use experimental :rakuast;

# A user subclass of a RakuAST node class is built from the same positional
# arguments as its parent (`Term::Declaration.new($declaration)`), as
# Grammar::BNF's `Slang::Declaration` is; and a `Declaration::Import` a module
# builds answers the value it was given.

plan 5;

my class Decl is RakuAST::Term::Declaration {
    method needs-sink-call(--> False) { }
}

my $constant = RakuAST::Declaration::ResolvedConstant.new(:compile-time-value(42));
my $decl = Decl.new($constant);
is $decl.^name, 'Decl', 'the subclass takes its parent constructor\'s positional argument';
ok $decl ~~ RakuAST::Term::Declaration, 'and is the parent node class';
nok $decl.needs-sink-call, 'its own method answers';

my Mu $stub := Metamodel::PackageHOW.new_type(:name<Stubby>);
my $import = RakuAST::Declaration::Import.new(:lexical-name<Stubby>, :compile-time-value($stub));
is $import.lexical-name, 'Stubby', 'an Import keeps its lexical name';
ok $import.compile-time-value =:= $stub, 'and its compile-time value';

# vim: expandtab shiftwidth=4
