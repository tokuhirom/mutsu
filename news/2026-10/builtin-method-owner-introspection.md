# Built-in method lists follow their declaring owners

Built-in `.^methods` now omits inherited names such as `List.map` and `Array.map`. The same names remain callable and visible through `.^methods(:all)`. A Rakudo snapshot check prevents the row metadata from claiming methods on the wrong owner again.
