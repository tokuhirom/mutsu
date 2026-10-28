# `my %args := %$args` is not a self-initialization

Binding a hash to the deref of a same-named scalar was rewritten to `HashVar(args)` and then
rejected as `my %args := %args` ("Cannot use variable %args in declaration to initialize itself").
That one spelling now keeps the `.hash` copy (a TODO in the parser says what the proper fix is).
Found loading Kind::Subset::Parametric; the rest of its suite needs #15271.
