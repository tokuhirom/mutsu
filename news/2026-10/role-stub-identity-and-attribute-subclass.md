# A role body's stubbed class keeps one identity; an `Attribute` subclass is a metaobject

Two fixes found while re-measuring PDF 0.6.17 (#12907).

A `my class A {...}` stub in a role body, followed by its definition, is one
class. When the role body ran again for a composition, the stub found the
class already defined and registered nothing, so the definition no longer
counted as its completion and was registered as a second class. A type
constraint written against the stub (`has A $.a`) then rejected instances of
the definition: "expected R::A but got R::A". The scope now records that the
stub statement ran and waits for its definition (`lexical_class_awaiting`),
whether or not the registry still holds a stub. PDF's
`Type check failed in assignment to $!cos; expected PDF::COS::Tie::COSAttr`
is gone.

A user subclass of `Attribute` (`my class CosOfAttr is Attribute does
COSAttrHOW {}`) records the facts given to `.new(:name, :type, :package)`,
and the `Attribute` methods (`name`, `type`, `package`, `get_value`, ...)
answer it after its own methods. `Attribute.new` and the subclass share one
argument reader.

Filed from the same run: #14143 (a module loaded by `require` inside a routine
loses its file-scope lexicals) and #14144 (`$v does R` on a readonly parameter
holding an Int does not reach the caller's container).
