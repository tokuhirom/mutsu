use Test;

plan 5;

# EVAL with an unsupported :lang raises X::Eval::NoSuchLang carrying `lang`.

throws-like 'EVAL("foo", :lang<no-such-language>)', X::Eval::NoSuchLang,
    lang => 'no-such-language';

throws-like 'EVAL("foo", :lang<no-such-language>)', X::Eval::NoSuchLang,
    message => /:s No compiler available for language/;

throws-like 'EVAL("1", :lang<python>)', X::Eval::NoSuchLang, lang => 'python';

# EVAL of Raku still works, with or without an explicit :lang<raku>.
is EVAL('1 + 2'), 3, 'plain EVAL works';
is EVAL('3 * 4', :lang<raku>), 12, 'EVAL :lang<raku> works';
