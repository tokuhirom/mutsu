# RakuAST: origins of declaration call chains, dotted aliases, constant traits

Three `.origin` gaps closed (#15780):

- a chain of calls on a declaration (`my $n.WHAT.^name`, `my $n.WHAT.Str.say`)
  now places the inner application from the start of the text to the dot of
  the outer call (a meta-method's name is a plain string);
- a regex alias to a non-capturing call with arguments
  (`<alias=.word("a")>`) reads past the dot;
- a constant's traits (`my constant \C is export(:tag) = 1`) are read after
  its name, sharing the trait reader of variable declarations
  (`decl_origins::read_traits`), and Rakudo's visit order puts the
  initializer before them.
