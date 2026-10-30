use Test;
use lib 't/lib';
use DeclareFromExportHook <declare class>;

# `LEXICAL::<EXPORTHOW> := ...` in an EXPORT hook publishes a declarator whose
# name and HOW depend on the `use` arguments: the importer's parse knows the
# keyword, the hook can ask `$*LANG.know_how` / `$*LANG.how`, and the HOW is a
# role punned over the language's own metaclass (annotations' `annotation`).

plan 5;

noted Greeter {
    method hi { "hi" }
}

is Greeter.hi, "hi", 'the declarator the hook published declares a class';
like Greeter.HOW.^name, /^ 'AnnotatedHOW[' .* 'ClassHOW]' $/,
    'its metaobject is the role HOW over the language\'s class HOW';
is Greeter.HOW.notes, ("Greeter",), 'the role HOW\'s compose ran for it';
ok Greeter.HOW ~~ Metamodel::ClassHOW, 'and it is still a ClassHOW';

noted Other { }
is Other.HOW.notes, ("Other",), 'each declaration gets a metaobject of its own';
