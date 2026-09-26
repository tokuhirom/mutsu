# CSS::TagSet loads: tagged enum imports, `also is` in class bodies, mixin `bless`

The ecosystem distribution CSS::TagSet 0.1.4 was `blocked_load`: none of its three
provided modules would `use`. Getting them to load exposed seven separate interpreter
gaps along its CSS::* dependency chain. All seven are fixed here. Its test files now run
up to one remaining gap, filed as #9654: two packages' same-named enums share one
registry entry.

**Tagged enum values no longer shadow quote constructs.** A `use`d module's enum values
reach the importer's parse, so that `FOO ?? … !! …` knows `FOO` is a term. They used to
arrive whatever tag the `use` named, and whether the module declared them or only
imported them itself. CSS::Units declares
`my enum Time is export(:Time) « :s(1.0) :ms(0.001) »`, so `use CSS::Units;` turned
every later `$x ~~ s/a/b/;` into a division by `s`, a parse error. Two rules now apply:

- An enum exported only under explicit non-default tags reaches an importer's parse
  only when that `use` names a matching tag (or `:ALL`).
- A module's own imports are no longer handed on to its importers.

**`use` tag spellings.** `use M :&name, :tag` names the tag `name` (it used to stop the
tag list and drop everything after it). `use M:tag` is also fixed: an adverb glued to
the module name is an inert name adverb, as in rakudo, not an import tag.

**`also is` / `also does` with types a class body `use`s.** Three problems, all in
CSS::Module's grammars and actions, which are written this way:

- Role-stub checking ran before the body's `also is` parents were published, so the
  inherited implementations went unseen.
- A punned `also is Role` was composed but dropped from the MRO.
- A deferred parent was appended after parents that happened to be loaded already.
  That reordering made C3 merges of subclasses inconsistent.

**Role stubs** are now satisfied by an own or inherited `token`/`rule`/`regex` or
`proto method`, not only by ordinary methods.

**`self.bless` on a mixin type object.** Inside a user `method new` invoked as
`(Base but R).new(...)`, `self.bless` now blesses the base class and composes the
mixed-in roles, like the default `.new`.

**A module loaded from a role body imports into itself.** The role body's import target
stayed set while the loaded module's own body ran. Operators the module imported for its
own use landed in the role instead, and its `12pt` died with "Bogus postfix".
