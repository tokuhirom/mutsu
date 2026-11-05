# `run` keeps `:out($handle)` / `:err($handle)` redirects when the call splats a list

`run $*EXECUTABLE, |@include.map(...), 'script.raku', :out($fh), :err($fh2)` wrote
nothing to the handles: with a `|@list` among the arguments the handle variable
reached the process-option reader still wrapped in its container, so the
IO::Handle check failed and the child's output was discarded. The option readers
now look through the container. The `static` distribution's `t/14-cli.rakutest`
goes from 14 failures to 3 (the remaining ones are `-I`/`-M` forwarding).
