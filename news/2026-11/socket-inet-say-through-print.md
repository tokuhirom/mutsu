# `$socket.say(...)` writes through the socket's `print`

`IO::Socket::INET` declares no `say`: in rakudo `$sock.say("x")` is `Mu.say`,
which gists and joins its arguments, adds the newline and hands the text to
the invocant's own `print`. mutsu's `Mu.say`-with-arguments fallback only
recognized a `print` the program declared, so a socket died with "No such
method 'say'". A built-in class's own `print` now counts too.

Rakuish's `t/16-agent-serve.t` now matches rakudo.
