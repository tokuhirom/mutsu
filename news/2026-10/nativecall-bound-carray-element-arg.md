# A CArray element bound to a variable is passed to a native call as its value

`my $fd := @pipes[i][j]; c_close($fd)` — Shell::DSL's `run-pipeline` closing its pipe fds — handed the
native call `0`: a CArray element bound to a variable is an `IntPosRef` reference, and the argument
marshaller only unwrapped scalar containers. `close(0)` ran instead of `close(3)`, the pipe's write end
stayed open in the parent and the reader (`cat`) never saw EOF, so `.echo('!!') |> .cat` hung forever.
The marshaller now reads the element such a reference addresses.
