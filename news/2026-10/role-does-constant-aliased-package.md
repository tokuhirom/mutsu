# `role R does Alias::Role` resolves a constant package alias

`constant VTree = Terminal::Widgets::Volatile::Tree; role Node does VTree::Node { }` died with
`Unknown role: VTree::Node`. The declared-type-name resolver now applies the constant package
alias prefix, as the run-time term lookup already did. Terminal::Widgets' `t/00-use.rakutest`
loads as under Rakudo.
