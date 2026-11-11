# `.^find_method` honors a class's `FALLBACK`

Rakudo's `ClassHOW.find_method` asks the class's fallbacks before giving up,
so on a class that declares (or inherits) `FALLBACK`, `.^find_method($name)`
for a name the class lacks answers a dispatcher that calls `FALLBACK` with the
name in front of the arguments. mutsu answered `Mu`, so code probing for a
method and passing the answer on to an `Any` parameter died binding it — EEL
0.3's runner reports an unknown subpattern this way (`t/06-runner.t`, "missing
subpattern is rejected", #16888).

`.^find_method` now builds that dispatcher (a small closure over the name)
when the class has a `FALLBACK`; `:no_fallback` keeps the old answer, as in
rakudo.
