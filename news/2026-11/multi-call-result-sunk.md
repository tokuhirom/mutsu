# A statement call to a multi sinks its result

`multi increment($b is rw) { ($b + 1) does role { method sink { $b++ } } }; increment($a);`
now runs the returned object's `sink` method, as a call to a plain sub already did. The
sink decision looked up one routine by name and answered "no" for a multi; it now sinks
when every candidate of the multi hands back a fresh value, and stays unsunk when one
candidate is `is rw`/`return-rw`. Found by the raku-doc differential sweep
(`Language/perl-func.rakudoc`).
