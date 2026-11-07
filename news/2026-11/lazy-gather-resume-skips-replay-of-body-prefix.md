# A lazy gather resumed inside a nested loop no longer replays the loop body's prefix

`(gather { while @q { my $d = @q.shift; for 1..2 -> $e { take ... } } }).map(...)`
died with "Cannot shift from an empty Array": a take-limit suspension inside
the nested `for` made the enclosing `while` / `loop` re-run its body from the
first statement on resume, shifting again. The resumed iteration now re-enters
at the nested loop op. App::Ariza t/01-resources passes.
