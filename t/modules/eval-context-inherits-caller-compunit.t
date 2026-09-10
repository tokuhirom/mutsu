use v6;
use lib 't/lib';
use MONKEY-SEE-NO-EVAL;
use Test;
use EvalContextWidget;

# #7836: `EVAL $code, context => $ctx` compiles the snippet in `$ctx`'s frame,
# and a compunit is part of a lexical scope just as much as a package is: which
# modules a compunit `use`d decides which package-qualified names code written
# in it may use (#7797's visibility gate). So the EVAL unit's parent must be the
# CONTEXT's compunit, not whichever one happens to be running `EVAL`.
#
# The user-visible casualty was `Test`'s own `throws-like`, which passes
# `context => CALLER::` precisely so the snippet compiles in the test file's
# scope. Without this, the snippet was compiled as if `Test.rakumod` had written
# it -- and `Test` never `use`s the module under test -- so every qualified name
# inside a `throws-like` string went undeclared. That took three bundled
# batteries (DateTime::Parse, Encode, Log::Async) down with it.

plan 6;

# The baseline both halves share: this compunit did `use EvalContextWidget`, so
# both the imported and the qualified name are in scope when written directly.
is EvalContextWidget::make(3), 3, 'the qualified name works written directly';
is make(3), 3, 'the imported name works written directly';

# A plain EVAL, no context: compiled by this compunit, so it inherits this
# compunit's `use` grants.
is (try EVAL 'EvalContextWidget::make(4)'), 4,
        'a plain EVAL inherits the running compunit grants';

# `throws-like` with a code STRING is the reported shape: `Test.rakumod` EVALs
# it with `context => CALLER::`.
throws-like 'EvalContextWidget::make(-1)', Exception,
        'throws-like resolves a qualified name from the caller compunit',
        message => /'bad widget'/;

# The same for a qualified CONSTANT, not just a sub call: the two go through
# different resolution chokepoints in the VM (bareword term vs qualified call).
is (try EVAL 'EvalContextWidget::WIDGET-LIMIT'), 7,
        'a qualified constant resolves in a plain EVAL';

throws-like 'EvalContextWidget::WIDGET-LIMIT.no-such-method',
        Exception,
        'a qualified constant resolves under a throws-like context EVAL too',
        message => /'no-such-method'/;
