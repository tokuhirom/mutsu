# `%!h does R` mixes the role into the attribute's container

`submethod TWEAK { %!h does Logs }` composed the role and then discarded the result: `%!h.does(Logs)`
stayed False and a later `%!h = ...` never reached the role's `STORE`, although `my %h does Logs`
always worked. The `does` on an `@`/`%` attribute now publishes the mixed container back into the
attribute's cell, and the container-store path reads the attribute cell (not just `env`) when it looks
for a mixed-in container to route the assignment through `STORE` (#16014).
