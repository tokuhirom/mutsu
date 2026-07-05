use Test;

# .match(..., :ex) / :exhaustive returns every possible match: at every start
# position, every possible length, ordered by start ascending and (at each
# position) longest first. mutsu previously ignored the :ex adverb on .match
# (returning a single greedy match / nothing), and ordered the m:ex// form
# shortest-first.

plan 16;

# Fixed-length pattern: exhaustive == every start position
is "abcabc".match(/a.c/, :ex).elems, 2, 'fixed pattern: two matches';
is "abcabc".match(/a.c/, :exhaustive).elems, 2, ':exhaustive alias';
is "banana".match(/a.a/, :ex).elems, 2, 'overlapping fixed matches';
is "banana".match(/a.a/, :ex)>>.Str.join(","), "ana,ana", 'overlapping match text';

# Variable-length pattern: every length at every position
is "aaa".match(/a+/, :ex).elems, 6, 'a+ over aaa: six matches';
is "aaa".match(/a+/, :ex)>>.Str.join(","), "aaa,aa,a,aa,a,a",
    'longest-first ordering per position';
is "1234".match(/\d+/, :ex).elems, 10, '\d+ over 1234: ten matches';
is "1234".match(/\d\d/, :ex).elems, 3, 'two-digit exhaustive: three matches';

# Captures survive
is "abab".match(/(a)(b)/, :ex).elems, 2, 'captured exhaustive matches';
is "abab".match(/(a)(b)/, :ex)[0][0].Str, "a", 'capture accessible';

# Literal string pattern
is "hello".match("l", :ex).elems, 2, 'literal string :ex';

# Empty / no match
is "".match(/a*/, :ex).elems, 1, 'empty string matches a* once';
is "abc".match(/x/, :ex).elems, 0, 'no match is empty';

# :ex returns a List
is "aaa".match(/a+/, :ex).^name, "List", ':ex returns a List';

# The m:ex// form orders the same way
"aaa" ~~ m:ex/a+/;
is $/>>.Str.join(","), "aaa,aa,a,aa,a,a", 'm:ex// ordering';

# :g and :ov are unaffected
is "aaa".match(/a+/, :ov).elems, 3, ':ov still works';
