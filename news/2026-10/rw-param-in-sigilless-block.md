# An `is rw` param inside a live `\x` block is writable

`(1,2).map(-> \x { @a.map(-> $x is rw { $x *= 10 }) })` died with a bogus
`postfix:<++>` dispatch error: the inline rw map loop left the enclosing
sigilless `\x`'s readonly marker (same bare name) on the inner `$x` param.
The loop now drops the marker for the param's scope and restores it on exit.
Separately, a compound assignment (`*=`, `~=`, ...) to a readonly variable now
raises the plain-assignment error instead of naming `postfix:<++>` (#11700).
