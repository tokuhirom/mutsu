# A module that exports by enumerating its OWN compilation unit, which is the
# standard shape of a hand-written `sub EXPORT` (String::Utils, Array::Sorted::Util,
# ... all use it verbatim). It works only if `UNIT::` lists the unit's lexical
# routines while EXPORT is running -- see t/modules/unit-stash-lexical-routines.t.

my sub alpha() { "A" }
my sub beta()  { "B" }

my sub EXPORT(*@names) {
    Map.new: @names
      ?? @names.map({ UNIT::{"&$_"}:p })
      !! UNIT::.grep({ .key.starts-with('&') && .key ne '&EXPORT' })
}
