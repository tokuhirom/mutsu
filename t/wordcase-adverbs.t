use Test;

# Str.wordcase accepts :where (which words to transform) and :filter (the
# transform, default tclc). mutsu previously only implemented the 0-arg form,
# so any adverb threw "No such method 'wordcase'".

plan 15;

# :where selects which words are title-cased
is "foo bar bazz".wordcase(:where(*.chars > 3)), "foo bar Bazz",
    ':where(*.chars > 3) title-cases only the long word';
is "hello world foo".wordcase(:where(*.starts-with("h"))), "Hello world foo",
    ':where(*.starts-with)';
is "foo123 bar".wordcase(:where(* ~~ /\d/)), "Foo123 bar",
    ':where with a regex matcher';

# :filter is the per-word transform
is "hello world".wordcase(:filter(*.uc)), "HELLO WORLD", ':filter(*.uc)';
is "a b c".wordcase(:filter({ $_ ~ "!" })), "a! b! c!", ':filter with a block';

# combined
is "abc def".wordcase(:filter(*.uc), :where(*.chars == 3)), "ABC DEF",
    ':filter + :where';
is "one two three".wordcase(:filter(*.uc), :where(*.chars > 3)), "one two THREE",
    ':filter applied only to matching words';

# the plain 0-arg form is unchanged
is "hello world".wordcase, "Hello World", 'bare wordcase';
is "HELLO".wordcase, "Hello", 'wordcase lowercases the tail';
is "aBc dEf".wordcase, "Abc Def", 'wordcase normalizes mixed case';

# hyphenated / apostrophe words are single words
is "the-quick brown".wordcase, "The-quick Brown", 'hyphen keeps one word';
is "it's a test".wordcase, "It's A Test", 'apostrophe keeps one word';

# punctuation / spacing preserved
is "  spaced,  out ".wordcase(:filter(*.uc)), "  SPACED,  OUT ",
    'non-word runs preserved';

# empty string
is "".wordcase(:filter(*.uc)), "", 'empty string';

# no word matches :where -> unchanged
is "a b c".wordcase(:where(*.chars > 5)), "a b c", 'no matching word is a no-op';
