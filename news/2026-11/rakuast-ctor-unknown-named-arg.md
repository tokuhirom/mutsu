# RakuAST constructors refuse an unknown named argument

A RakuAST named constructor accepted a named argument the class does not take
and ignored it (`RakuAST::Doc::Block.new(type => 'para', bogus => 1)`); rakudo
dies with `Unexpected named argument 'bogus' passed`, and so do they now. The
check is against each constructor's own list of names, which was compared with
the signatures rakudo 2026.09 declares: the two names rakudo accepts that mutsu
does not keep (`Statement::For`'s `otherwise`, which rakudo drops as well, and
`Type::Subset`'s `WHY`, filed as #16773) are let through explicitly (#16771).
