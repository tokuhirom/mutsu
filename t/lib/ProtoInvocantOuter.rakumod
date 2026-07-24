unit class ProtoInvocantOuter;

use ProtoInvocantInner;

has ProtoInvocantInner $.inner = ProtoInvocantInner.new;

# A plain method that merely delegates to the attribute's cross-module proto
# method. Nothing here mutates the outer invocant.
method poke($f) { $.inner.field($f) }

method poke-named(*%h) { $.inner.field(|%h) }
