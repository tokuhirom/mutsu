use Test;

plan 4;

is-deeply "<>[]()".comb(/.<(.)>/).Seq.list, ("<>".substr(1), "[]".substr(1), "()".substr(1)),
    '.comb honours <( in the regex';
is-deeply "abcd".comb(/a<(b)>/).Seq.list, ("b",), '.comb honours <( and )>';
is-deeply "abab".comb(/a<(b)>/, 1).Seq.list, ("b",), '.comb with a limit honours the markers';
is-deeply "abcd".comb(/b../).Seq.list, ("bcd",), 'plain regex is unchanged';
