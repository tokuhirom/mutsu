use v6.e.PREVIEW;
module ExportMandatory::Core {
    our only infix:<mandatory-cat>(Mu $a, Mu $b) { "$a+$b" }
}
my package EXPORT {
    OUR::<DEFAULT> := ExportMandatory::Core;
}
