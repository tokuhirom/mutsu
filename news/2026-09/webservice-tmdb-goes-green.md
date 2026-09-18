# WebService::TMDB passes its ecosystem suite

The `WebService::TMDB` 0.1.2 distribution was selected by the ecosystem
roulette and locked on #7884. Its mutsu baseline moved from 2 of 4 files to
4 of 4: the request and full TMDB suites now pass as well as the load and
user-agent tests.

The fixes cover the interpreter gaps exposed by the distribution: MOP
composition now returns its composed type and invalidates the cached MRO when
parents change; named hash parameters materialize nested pairs correctly;
sigilless type-object and raw bindings preserve their source type through
nested JSON decoding; and calls such as `Array[Int]([45, 47])` use the existing
typed aggregate constructor path.

The latter syntax is pinned by
`t/routines/signature/parameterized-call.t`; the other reductions are covered
by the focused signature and MOP tests added alongside the fixes.
