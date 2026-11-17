# A run-time method name takes the same fast lanes as a bare one

`$obj."$name"()` — a method name computed at run time, the idiom lizmat's
modules use to walk an object's attributes — was refused by the accessor lane,
the plain-method lane and the fast accessor read, because a quoted name can
mean something different from the bare one. It only does for a MOP
pseudo-method name (`WHAT`, `HOW`, `VAR`, ...), which a quoted call sends to a
user method of that name instead of the reflection macro. For every other name
the quoted call now takes and installs the same lanes, so a dynamic accessor
read went from 2.8 us to 0.35 us.

Three smaller costs on the same paths went too: a type-smiley bareword such as
`Str:D` in `$x ~~ Str:D` is remembered per site (while its base name is not
rebound) instead of being resolved on every execution; Mu's default rendering
fallback leaves at once for any method other than `raku`/`gist`/`perl`, before
its role probes; and the `is rw` lookup a bound accessor read (`my $v :=
$o."$name"()`) takes is memoized instead of cloning every attribute
definition. On a loop of those three shapes, callgrind counts 1,696.8M
instructions before and 552.6M after.

SBOM::CycloneDX's `t/90-valid.rakutest` (#17811), where these shapes were found,
is not measurably faster yet: its remaining cost is spread over a declaration
bind (#17874), a `sub` declared inside a `map` block (#17875) and type
smartmatches against built-in receivers (#17876).
