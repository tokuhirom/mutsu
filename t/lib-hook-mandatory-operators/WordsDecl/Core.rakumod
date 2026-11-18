unit module WordsDecl::Core;
our sub infix:<wdc>($a, +@b) is looser<...> { "wdc($a; {@b.join(',')})" }
