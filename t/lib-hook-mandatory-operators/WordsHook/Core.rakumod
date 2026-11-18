unit module WordsHook::Core;
our sub infix:<whk>($a, +@b) is looser<...> { "whk($a; {@b.join(',')})" }
