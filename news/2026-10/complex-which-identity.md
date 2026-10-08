# Complex.WHICH matches Rakudo

`(1+2i).WHICH` is now `Complex|1|2` (type, real, imaginary separated by `|`) instead of `Complex|1+2i`, in both `.WHICH` and the shared identity key. Closes #12304.
