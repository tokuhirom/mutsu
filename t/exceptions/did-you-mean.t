use Test;

plan 16;

# "Did you mean" suggestions for misspelled methods follow Rakudo's
# X::Method::NotFound: candidates are the invocant's whole method set (its MRO
# and composed roles), ordered by edit distance then name, wrapped at 72 columns.

sub msg(&code) {
    my $m = '';
    try { code(); CATCH { default { $m = .message } } }
    $m
}

is msg({ "hello".upercase }), "No such method 'upercase' for string 'hello'",
    "a Str invocant reads 'for string'; 'uppercase' is not a Raku method";
is msg({ [1,2,3].pus(4) }),
    "No such method 'pus' for invocant of type 'Array'. Did you mean any of\nthese: 'push', 'put'?",
    "several candidates are joined and wrapped";
is msg({ 42.squirt }),
    "No such method 'squirt' for invocant of type 'Int'. Did you mean any of\nthese: 'sqrt', 'squish'?",
    "Int draws on Any/Mu/Cool methods too";
is msg({ "hello".xyz }), "No such method 'xyz' for string 'hello'", "no suggestion for 'xyz'";
is msg({ my %h = a => 1; %h.keypairs }),
    "No such method 'keypairs' for invocant of type 'Hash'", "nothing within range on Hash";

# #11816: inherited Any methods are candidates
is msg({ 5.lazy }),
    "No such method 'lazy' for invocant of type 'Int'. Did you mean any of\nthese: 'any', 'say'?",
    "inherited Any::any is suggested";
is msg({ "x".lazy }),
    "No such method 'lazy' for string 'x'. Did you mean any of these: 'any',\n'say'?",
    "Str: 'for string', and the wrap is on the whole sentence";
is msg({ 5.sayy }),
    "No such method 'sayy' for invocant of type 'Int'. Did you mean any of\nthese: 'say', 'any'?",
    "ordered by distance, then by name";
is msg({ 5.elemz }),
    "No such method 'elemz' for invocant of type 'Int'. Did you mean\n'elems'?",
    "a single candidate reads 'Did you mean'";
is msg({ 5.sya }), "No such method 'sya' for invocant of type 'Int'",
    "'say' is two edits away from a 3-letter name: too far";
is msg({ 5.sy }),
    "No such method 'sy' for invocant of type 'Int'. Did you mean any of\nthese: 'say', 'so'?",
    "short names allow one edit";
is msg({ 5.prin }),
    "No such method 'prin' for invocant of type 'Int'. Did you mean any of\nthese: 'print', 'asin', 'join', 'min', 'pred', 'printf', 'sin', 'trim'?",
    "ties at the boundary distance are all listed";
is msg({ 5.ant }),
    "No such method 'ant' for invocant of type 'Int'. Did you mean any of\nthese: 'AST', 'Int', 'any', 'int'?",
    "case-insensitive distance, codepoint order";
is msg({ [1].length }),
    "No such method 'length' for invocant of type 'Array'. Did you mean\n'elems'?",
    "length on a List suggests elems";
is msg({ Str.lazy }),
    "No such method 'lazy' for string ''. Did you mean any of these: 'any',\n'say'?",
    "the Str type object reads 'for string \"\"'";
is msg({ 5.ceil }),
    "No such method 'ceil' for invocant of type 'Int'. Did you mean any of\nthese: 'ceiling', 'Real', 'cis', 'perl', 'tail'?",
    "ceil lists ceiling first, then the other candidates in range";
