# Terminal::Widgets sigilless ternary cache initializer

mutsu now parses assignments in the else branch of a ternary used by a
sigilless declaration initializer. This fixes the `%colors<name> //= color`
shape in `Terminal::Widgets::Widget` and unblocks the distribution's parser
from reaching the following `for` block.

Closes #7999.
