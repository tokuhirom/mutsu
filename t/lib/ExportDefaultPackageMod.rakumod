# `OUR::<DEFAULT> := Pkg` in `my package EXPORT` exports Pkg's whole symbol
# table (annotations::containers, #15259).
module ExportDefaultPackageMod {
    our sub exported-sub() { "sub" }
    OUR::<ExportedClass> := class ExportedClass { method hi { "hi" } };
    my class Hidden { method who { "hidden" } }
    OUR::<Aliased> := Hidden;
}

my package EXPORT {
    OUR::<DEFAULT> := ExportDefaultPackageMod;
}
