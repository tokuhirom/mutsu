# A text read of a Proc pipe turns CR-LF into LF

`run(..., :out).out.slurp` returned a child's `"x\r\ny"` untouched, while a file handle (and
rakudo's pipe) reads the CR-LF grapheme as `"\n"`. The pipe's captured text is now normalised
the same way (binary pipes keep their bytes). Found with Prompt::Hidden (t/01-plain.t).
