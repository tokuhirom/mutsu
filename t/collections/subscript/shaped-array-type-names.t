use Test;

# Rakudo 2026.11's shaped-array rework declares these types in the setting
# (#19274). Their behaviour (jagged shapes, row views, tuple-keyed hashes) is
# #19282 and #19283; the names already resolve to their type objects.

plan 15;

for Array::Jagged, Array::JaggedRow, Array::JaggedRows, Array::JaggedSlice,
    Array::JaggedSlots, Array::ShapedView, Hash::Shaped -> \T {
    nok T.DEFINITE, "{T.^name} is a type object";
}

is Array::Jagged.^name, 'Array::Jagged', 'Array::Jagged.^name';
is Array::ShapedView.^name, 'Array::ShapedView', 'Array::ShapedView.^name';
is Hash::Shaped.^name, 'Hash::Shaped', 'Hash::Shaped.^name';
is Array::ShapedView.gist, '(ShapedView)', 'gist is the short name';
is Hash::Shaped.gist, '(Shaped)', 'gist of Hash::Shaped';
is Array::JaggedRows.raku, 'Array::JaggedRows', 'raku is the full name';
ok Array::ShapedView ~~ Any, 'a class under Any';
is-deeply Array::ShapedView.WHAT.^name, 'Array::ShapedView', 'WHAT keeps the name';
