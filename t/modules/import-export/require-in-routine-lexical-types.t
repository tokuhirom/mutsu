use Test;
use lib $?FILE.IO.parent(3).add('lib').Str;

plan 5;

# A module loaded by `require` inside a routine keeps its file-scope lexical
# types after the routine returns (#14143). A `my class` nested in another
# `my class` is stored as `Outer\0<id>::Inner\0<id>`; the type lookup used to
# miss it, and picked the nested type when asked for the outer one.
sub load($pkg) { require ::($pkg) }

my $m = load('RequireInRoutineLexTypes');
my $attr = $m.attr;
is $attr.of-att, 'cos-of', 'a method sees the type nested in its class';
is $attr.names, 'a,b,c', 'a file-scope constant array';
is $attr.locked, 'locked', 'a file-scope constant object';
is $m.outer.m, '7 inner', 'a file-scope variable and a nested type';
is $attr.^name, 'RequireInRoutineLexTypes::COSAttr', 'the outer type, not the nested one';
