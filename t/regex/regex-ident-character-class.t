use Test;

plan 8;

# In a character class, <ident> denotes one identifier-start character.  The
# subtraction form is used by ECMA262Regex's parser dependency.
grammar IdentifierClass {
    token TOP { <-ident-[\c[ZWJ]\c[ZWNJ]]> }
}

nok IdentifierClass.parse('a'), '<-ident> rejects alphabetic characters';
nok IdentifierClass.parse('_'), '<-ident> rejects underscore';
ok IdentifierClass.parse('0'), '<-ident> accepts digits';
ok IdentifierClass.parse('!'), '<-ident> accepts punctuation';

is (rx/a/).gist, 'rx/a/', 'rx// preserves its source form';
is (/a/).gist, '/a/', 'slash-delimited regex keeps its source form';
ok 'FOO' ~~ rx:Perl5:ignorecase/^foo$/, 'Perl5 ignorecase affects matching';
is (rx:Perl5:ignorecase/^foo$/).gist, 'rx:Perl5:ignorecase/^foo$/',
    'Perl5 regex preserves its source adverbs';
