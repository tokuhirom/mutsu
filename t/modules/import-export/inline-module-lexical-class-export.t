use Test;

# A lexical `my class C is export` in an INLINE module is exported like an
# `our` class: `import M` brings it into scope by its short name (#10557).
# Expected values checked against rakudo.

plan 6;

module M1 { my class CC is export { method m { 42 } } }
{
    import M1;
    is CC.m, 42, 'my class ... is export is imported';
}

module M2 {
    my class CC2 is export { method m { 43 } }
    sub f2 is export { 'f' }
}
{
    import M2;
    is f2(), 'f', 'a sibling exported sub still imports';
    is CC2.m, 43, '... and so does the lexical class next to it';
}

module M3 { my class CC3 is export(:t) { method m { 44 } } }
{
    import M3 :t;
    is CC3.m, 44, 'a tagged lexical class imports with its tag';
}

module M4 { sub g { my class CC4 is export { method m { 45 } } } }
{
    import M4;
    is CC4.m, 45, 'a lexical class declared in a routine of the module';
}

module M5 { my class CC5 is export { method m { 46 } } }
{
    import M5;
    is CC5.^name, 'M5::CC5', 'the imported class keeps its qualified name';
}
