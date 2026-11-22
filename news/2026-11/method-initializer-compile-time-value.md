# `Method::Initializer.compile-time-value`

An attribute default now carries rakudo's `RakuAST::Method::Initializer` thunk,
but asking the thunk for its `.compile-time-value` died with "No such method".
static's analyzer asks that of every routine node at CHECK time, so a program
with an attribute default (`has Int $.x = text()`) aborted the analysis instead
of reporting the mismatch. The thunk now answers with the anonymous method it
stands for, built from the node once, as rakudo does (#18814).
