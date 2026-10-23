unit module AttrDefaultConstSrc;
constant SUPV is export = (1, 2);
constant SUP-S is export = "ab";
sub helper() is export { 42 }
enum E is export (EA => 1);
