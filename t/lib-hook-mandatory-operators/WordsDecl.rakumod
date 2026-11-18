# The same, from a hook that may hand its importer declarators through
# `LEXICAL::<EXPORTHOW>`: such a module is activated for the importer's parse
# like a slang, and the activation reports its routines (#18091).
module WordsDecl { use WordsDecl::Core; }
my package EXPORT {
    package MANDATORY {
        OUR.WHO.BIND-KEY: .key, .value for WordsDecl::Core::;
    }
}
sub EXPORT(*@args) {
    LEXICAL::<EXPORTHOW> := Metamodel::PackageHOW.new_type(:name<EXPORTHOW>)
        if @args && @args[0] eq 'declare';
    Map.new
}
