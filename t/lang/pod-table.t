use Test;
plan 8;

=table
X | O |
---+---+---
  | X | O
---+---+---
  |   | X

=table
X | O | 
---+---+---
  | X | O
---+---+---
  |   | X

is $=pod.elems, 2, 'captures two pod tables';
isa-ok $=pod[0], Pod::Block::Table, 'first entry is Pod::Block::Table';
is $=pod[0].contents.elems, 3, 'first table has three rows';
is $=pod[0].contents[0].join(','), 'X,O,', 'first row keeps trailing empty cell';
is $=pod[0].contents[1].join(','), ',X,O', 'second row parsed correctly';
is $=pod[0].contents[2].join(','), ',,X', 'third row parsed correctly';
is $=pod[1].contents[0].join(','), 'X,O,', 'trailing whitespace after ending pipe is accepted';
is $=pod[1].contents[2].join(','), ',,X', 'second table data row parsed correctly';
