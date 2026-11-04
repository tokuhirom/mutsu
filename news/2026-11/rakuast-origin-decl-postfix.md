# RakuAST: origins of a declaration used as a postfix operand

`my $k.defined` (and `is my $k.defined, False`) now gives the declaration the
range of `my $k`, and the method call, its `Name` and its `ArgList` theirs, as
rakudo does (#15780). A longer chain over a declaration
(`my $n.WHAT.^name`) is still without ranges for its inner application.
