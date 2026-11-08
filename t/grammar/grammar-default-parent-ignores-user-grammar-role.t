use Test;

# A user role group named `Grammar` (EEL's `EEL::Slang::Grammar` is the same
# shape under a package) must not be mistaken for the core `Grammar` class every
# `grammar` inherits by default (#16888).

plan 6;

our role Grammar[Mu $x] {
    method kind { "role of {$x.raku}" }
    token word { <[a..z]>+ }
}

lives-ok { EVAL 'grammar Plain { token TOP { \d+ } }' },
    'declaring a grammar next to a role Grammar[...] lives';

grammar Declared { token TOP { <[a..z]>+ } }

ok Declared.parse('abc'), 'the grammar parses';
is Declared.^parents.map(*.^name).join(','), 'Grammar,Match,Capture',
    'its default parent is the core Grammar';

is Grammar[5].^name, 'Grammar[Int]', 'the role instance still resolves';
is Grammar[5].new.kind, 'role of 5', 'and works';

class Pun does Grammar[7] { }
is Pun.new.kind, 'role of 7', 'a class can still do the role explicitly';
