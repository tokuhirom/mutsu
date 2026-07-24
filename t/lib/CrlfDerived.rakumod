use CrlfBase;
unit class CrlfDerived is CrlfBase;
# Same-named module-level scalar as the parent. Before the fix, the parent's
# `my $CRLF` leaked into the persistent mainline env, so this class's own
# `my $CRLF` was not registered as a class-body static and the method below fell
# back to whatever last wrote the bare `CRLF` global.
my $CRLF = "derived-crlf";
method derived-crlf() { $CRLF }
