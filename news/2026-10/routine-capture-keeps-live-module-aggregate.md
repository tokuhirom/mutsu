# A named routine no longer re-installs a registration-time aggregate capture

The closure-capture flip (#14469) overwrote the caller's same-named binding
with the whole plain-lexical capture, also for a named routine. A routine's
capture is taken when it is registered, before a file-scope `my %seen{Any}`
declared after it has run, so for `PDF::COS::Util` the "captured" `%seen` was
another compunit's `%seen{Hash}` and every `to-ast` died with a key type
check. A routine now keeps the live `@`/`%` binding. PDF 0.6.17
`cos-type-xref` passes again.
