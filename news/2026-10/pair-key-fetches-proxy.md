# A Proxy used as a Pair key is FETCHed

`$proxy => 1` and `@proxies Z=> @values` kept the Proxy itself as the key, so a
hash or `Map` built from them was keyed by the text `Proxy`. The key of `=>` is
a decontainerized `Mu $key`, so the Proxy is now FETCHed once when the pair is
made (including a slot `BIND-POS`-bound to the Proxy). The `annotations`
distribution's `Keeper.hash` zips its symbols this way.
