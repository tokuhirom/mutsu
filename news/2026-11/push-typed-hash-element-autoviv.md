# `push %h{$k}, $x` autovivifies a typed hash slot with its element type

The sub form of `push`/`unshift`/`append`/`prepend` on an indexed slot was
compiled as `slot = slot // []; slot.push(...)` plus a write-back, so a hash
declared with an array element type (`has Array[Dist] %!lookup`) refused the
plain `[]` it stored. The slot is now the target of the ordinary method call,
whose vivification already creates the declared element type and writes a
nested slot back. This unblocks Zef's `Ecosystems.populate-distributions`,
and with it `mzef install`. Issue #16115.
