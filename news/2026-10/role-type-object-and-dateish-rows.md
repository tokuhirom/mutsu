# The roles' type-object coercions and `Dateish`'s renderings are rows

Two more by-name gates of the slow-path dispatcher are role methods now (ADR-11276 §9.181):
`.Numeric`/`.Real` of the type object of a class composing `Numeric`/`Real` (Rakudo's `:U`
candidates: warn, then `self.new`) and `.Str`/`.gist` of a class composing `Dateish` (its
private `!formatter`). Answers are unchanged.
