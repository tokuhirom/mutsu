unit module ModuleMethodLexical;

our sub helper($value) { "module:$value" }

class GLOBAL::ModuleMethodLexicalTarget {
    has $.value = 5;

    method declared() { helper($!value) }
}

class GLOBAL::Unrelated::ModuleMethodLexicalTarget {
    has $.value = 6;

    method declared() { helper($!value) }
}

ModuleMethodLexicalTarget.^add_method(
    'added',
    method () { helper($.value) },
);
