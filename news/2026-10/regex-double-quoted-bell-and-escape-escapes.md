# `"\e"` and `"\a"` in a double-quoted regex literal

A double-quoted literal inside a regex handled `\n \t \r \f \b \0 \c[..] \x..` but not `\e` or `\a`:
`"\e"` matched the letter `e` and `"\a"` the letter `a`. Selkie's `t/82-app-facade.rakutest` matches a
tmux passthrough sequence with `/"\ePtmux;" .* "\e\e]0;Hi\a" .* "\e\\"/` and failed on it. Both
escapes now produce ESC and BEL.
