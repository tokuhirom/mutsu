my class Foo { method hi { "hi" } }
module OurBindLexicalClass {
    OUR::<Foo> := Foo;
}
my package EXPORT {
    OUR::<DEFAULT> := OurBindLexicalClass;
}
