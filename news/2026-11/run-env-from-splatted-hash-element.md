# `run |@cmd, |%opts` honours an `env` stored as a hash element

`%opts<env> = %env; run |@cmd, |%opts` gave the child an empty environment: the
splatted element arrives as a container reference and `run` coerced that
instead of the hash inside it. `run` now looks through the container.
App::Ariza t/14, t/19 and t/21 match raku.
