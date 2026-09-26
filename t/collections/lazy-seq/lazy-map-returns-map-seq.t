use Test;

# #9619: a lazy map pipeline (infinite source) whose callback returns a
# `.map`/`.grep` Seq must reify that Seq, so static readers (gist, `.raku`,
# `.flat`) see its elements instead of `()`.

plan 9;

is (1..*).map({ (1..$_).map(* * 2) }).head(3).gist, '((2) (2 4) (2 4 6))',
    'gist of .map Seq elements from a lazy pipeline';
is (1..*).map({ (1..$_).map(* * 2) }).head(3).raku,
    '((2,).Seq, (2, 4).Seq, (2, 4, 6).Seq).Seq',
    '.raku of .map Seq elements from a lazy pipeline';
is (1..*).map({ (1..$_).map(* * 2) }).head(3).flat.gist, '(2 2 4 2 4 6)',
    '.flat of .map Seq elements from a lazy pipeline';
is (1..*).map({ (1..$_).grep(* > 0) }).head(2).gist, '((1) (1 2))',
    '.grep Seq elements from a lazy pipeline';
is (1..*).map({ (1, 2).map(* * 2) }).head(2).gist, '((2 4) (2 4))',
    '.map over a list literal from a lazy pipeline';
is (1..*).map({ (1..$_).map(* * 2) })[1].gist, '(2 4)',
    'indexing still works';

my \s = (1..*).map({ (1..$_).map(* * 2) }).head(2);
is s.gist, '((2) (2 4))', 'rendering the element once';
is s[1].sum, 6, 'element is still readable after rendering';

is (1..*).map({ (1..Inf).map(* + 0) }).head.head(2).gist, '(1 2)',
    'a callback returning an infinite .map stays lazy';
