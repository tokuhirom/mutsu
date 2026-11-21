# RakuAST: origins of a hyper assignment

A hyper assignment such as `@a >>+=>> 1` or `($a, $b) »~=» <p q>` is a
`MetaInfix::Assign` over a `MetaInfix::Hyper`, and mutsu placed neither the
application nor its operators (#15780). As in Rakudo, the assignment now
spans the markers, the operator and the `=`, and the `Infix` inside is placed,
while the hyper node itself gets no origin.
