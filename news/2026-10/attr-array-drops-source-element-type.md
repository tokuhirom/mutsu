# `@` attributes no longer inherit a source array's element type

Handing an `Array[Str]` to an untyped `has @.columns` (through `.new(:columns(@typed))` or a
default) kept the `Array[Str]` tag, so `$o.columns eqv ['a']` was False. The attribute now owns a
plain `Array`, as in Rakudo. Found with DB::Migration::Declare's `t/model-check.rakutest`, which
now passes 59/59.
