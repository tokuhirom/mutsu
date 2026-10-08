# Typed array attribute accessors keep the stored array's identity

`has Str:D @.values` set through `self.bless` was copied on every accessor
read, so `$obj.values.append: ...` (as `HTTP::Header::Strict.push-field` does)
never reached the attribute. The fast accessor read re-tagged an already typed
container whose metadata spelling differed (`Str` vs `Str:D`), and `bless`
dropped the definedness smiley when typing the attribute. The accessor now
leaves a typed container alone and `bless` records `Array[Str:D]` like `.new`.
