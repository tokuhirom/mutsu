# RakuAST: origins of constructs the roast corpus showed unplaced

The `.origin` differential (#15780) was run over a sample of whitelisted
roast files, real Raku code rather than the focused `t/rakuast/` tests, and
the gaps it showed are closed:

- a variable or array composer right after a contextualizer's sigil
  (`$@a`, `%$hash`, `$[ 21 ... 25 ]`);
- a qualified name called as a bare word (`M::foo if True`): its `Name`
  spans the call and its empty `ArgList` sits at the end;
- a declaration a compound operator assigns to (`my $i **= $i`);
- an imaginary rational written from its point (`.5i`);
- a call that is a prefix's operand (`!not(not 0)`);
- a method call assigned to (`$pair.value += 100`);
- a term called as a method (`$o.$meth`), which now visits its empty
  `ArgList`, and an empty anonymous routine signature (`method () { }`);
- an assignment under a statement prefix (`sink $i = 5`, `try $i = 5`);
- a hyper subscript (`%h{*}»[1]`) and the `<?>` / `<!>` regex assertions;
- the empty `ArgList` of a call in a one-line anonymous body, whose origin a
  fragment copy dropped.

Two visit shapes now match Rakudo too: a `Name` visits no `Name::Part::Simple`
(only the expression of an indirect part and an operator's word quote), and
an empty `ArgList` keeps its origin through a fragment copy.
