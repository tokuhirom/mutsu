use Test;
use lib $?FILE.IO.parent(2).add('lib');
use RegexDeclClassLexical :FORMAT;

# A class-body `my regex` interpolating a body lexical keeps that lexical when
# it is used from another compunit (#11292).
plan 2;

is fmt("[%P] %D"), "[<P>] <D>", 'subst with &regex sees the class-body lexical';
is fmt-match("[%D]"), "%D", 'match with &regex sees the class-body lexical';
