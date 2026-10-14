# RakuAST::Trait::Is can be constructed by hand

`RakuAST::Trait::Is.new(name => ..., argument => ...)` used to die with "Could not find symbol '&Is'". It now builds the same node the `.AST` direction produces, omits an absent `argument` like Rakudo, and a hand-built `is` trait on a hand-built class is honoured by `EVAL`. `Trait::Will` and `Trait::Trusts` remain unregistered classes (#13133).
