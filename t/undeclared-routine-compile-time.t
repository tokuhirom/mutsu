use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

# Compile-time undeclared-routine detection (rakudo's CHECK-time
# X::Undeclared::Symbols). Pins the mainline pre-execution check added with
# src/runtime/undeclared_routines.rs.

plan 8;

# The error fires before ANY mainline code runs: `say 42` must not print.
is_run 'say 42; nosuchsub()', {
    :out(''),
    :status({ $_ != 0 }),
    :err(/nosuchsub/),
}, 'undeclared routine is caught before run time (say does not run)';

# The reported line is the call site inside the sub body, not line 1.
is_run "\nsub bar \{\n    pfff();\n\}\n\nbar()", {
    :out(''),
    :status({ $_ != 0 }),
    :err({ $_ ~~ /pfff/ and $_ ~~ /3/ }),
}, 'call inside a sub body reports the body line';

# A case typo of a phaser gets the phaser suggested.
throws-like 'begin 42', X::Undeclared::Symbols, message => /'BEGIN'/,
    "begin suggests 'BEGIN'";

# A sub declared later in the unit is fine (routines resolve at CHECK time).
is_run 'f(); sub f { print "later" }', {
    :out('later'),
    :status(0),
    :err(''),
}, 'forward call to a later sub declaration is fine';

# A callable parameter is a declaration for calls inside the body.
is_run 'sub ap(&f) { f() }; ap({ print "cb" })', {
    :out('cb'),
    :status(0),
    :err(''),
}, 'callable parameter suppresses the check inside the body';

# Compiler-special callables (cas/atomic-*) never reach the runtime dispatch
# tables but must not be flagged.
is_run 'my $x = 1; cas $x, { $_ + 1 }; print $x', {
    :out('2'),
    :status(0),
    :err(''),
}, 'cas is a known callable';

# A lexical &-variable counts as a declaration.
is_run 'my &g = { print "amp" }; g()', {
    :out('amp'),
    :status(0),
    :err(''),
}, 'my &g declaration suppresses the check';

# A unit that imports names it cannot see through skips the check entirely
# (the call still fails, but only at run time — after `say` ran).
is_run 'use MONKEY-SEE-NO-EVAL; say 1; EVAL q|sub zzz9 { print "z" }|; zzz9()', {
    :status({ $_ != 0 }),
}, 'a use statement makes the unit skip the compile-time check';

# vim: expandtab shiftwidth=4
