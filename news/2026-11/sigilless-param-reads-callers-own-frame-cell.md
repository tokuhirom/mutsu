# A sigilless parameter no longer binds a grand-caller's same-named variable

Binding `\year` to the caller's `$y` promotes that variable to a shared cell,
and looked the cell up with `env.get("y")` along the whole chain. When the
caller's `$y` was still a plain local (not yet in the env) the lookup walked on
to an older frame's `$y` and made THAT the argument: after the call the caller's
`$y` read the grand-caller's value. Timezones::ZoneInfo's `timesub` hit it
(`isleap($y)` in a `while` condition after `time2sub` had called `isleap($y)`),
turning every local-time -> POSIX conversion into -1. The lookup now stays in
the caller's own frame (`Env::get_sym_in_frame`), so `posix-from-calendar` gives
rakudo's `1577934245`. Closes #16177.
