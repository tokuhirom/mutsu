# A computed `<::($n)>` regex call files no capture

`"a" ~~ / <::($n)> /` filed the call under the resolved name (`$<alpha>`); rakudo files nothing, and
neither does a grammar token with `<::($n)>`. Rakudo only files the call under its name when the
name is written out in the regex (`<::("x")>`, or a `~` of literals, which it folds); a name that has
to be computed (a variable, an interpolated string, a method call) is looked up on the cursor at run
time and behaves like `<.x>`: no capture of its own, nothing inside it visible in `.hash`, its action
still fires. `<q=::($n)>` captures the call under `q` alone, where mutsu also filed the resolved name.

The compiled engine now evaluates a computed-name call as the silent call `<.name>` and keeps the
named call for a constant name (`rx_symbolic_call_ends`, `symbolic_name_is_constant`).
