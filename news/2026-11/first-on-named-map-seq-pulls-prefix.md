Calling `.first` on a named Seq returned by `.map` now evaluates the map
callback only through the first match, including with `:k`, `:kv`, and `:p`.
