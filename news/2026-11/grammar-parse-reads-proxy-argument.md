# `Grammar.parse`/`subparse` read a Proxy argument

`Recipe.subparse($!recipe, ...)` with `$!recipe` an `is mooish` attribute (a Proxy) parsed the text `Proxy` instead of what the Proxy FETCHes, so Cooklang's parser stopped five characters in (`Unable to parse recipe ... near 'na cake'`). The grammar parse methods now FETCH a Proxy argument first.

Cooklang now parses its recipes. The remaining Cooklang failures come from `$param =:= TypeObject` for typed parameters (#16957) and an `is rw` accessor over a Proxy (#16958).
