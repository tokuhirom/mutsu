# `self.bless` on a role pun keeps the roles the role composes

`self.bless` inside a role's method, called on the type object of a role that itself
`does` another role (`role PathKey does Key[IO::Path]`, `PathKey.of: 'dir'`), returned a
bare class instance without the role markers whenever the method call had already
punned the role. The instance then failed `~~ Key` and a `multi` taking `Key` was
skipped. `dispatch_bless` now marks a pun instance like `.new` does. This is the last
difference in App::RaCoCo's `t/19-configuration.rakutest` that was not the test
environment (`$*EXECUTABLE` is not `raku`).
