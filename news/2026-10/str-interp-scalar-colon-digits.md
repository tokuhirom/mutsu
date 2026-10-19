# A colon and digits after an interpolated scalar stay literal text

`"$!port:8123"` and `"$v:8123"` read the scalar as a variable named `port:8123`
because the adverb-suffix scan accepted a digit-leading name after the colon. The
suffix must now start like an identifier, so the text after the colon stays
literal, as in Rakudo. This unblocks `Dev::ContainerizedService`'s ClickHouse spec
(`'-p', "$!port:8123"`), which died with "Attribute $!port:8123 not declared".
