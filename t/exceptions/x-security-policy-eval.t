use Test;

# #18937: a prohibited regex interpolation throws X::SecurityPolicy::Eval
# (payload "Prohibited regex interpolation"), and the class's message is
# rakudo's wrapped "use the MONKEY-SEE-NO-EVAL pragma" text, not a stub.

plan 6;

# `use Test` exports MONKEY-SEE-NO-EVAL, so the refused interpolation runs in
# its own process.
my $p = run $*EXECUTABLE, '-e',
    q[my $s = q<{1}>; try { "a" ~~ /<$s>/ }; .say for ($!.^name, $!.payload, $!.message)],
    :out, :err;
my @lines = $p.out.slurp(:close).lines;
$p.err.slurp(:close);
like @lines[0] // '', /^ 'X::SecurityPolicy::Eval' >>/,
    'regex interpolation of code throws X::SecurityPolicy::Eval';
is @lines[1], 'Prohibited regex interpolation', 'payload names the refused construct';
is @lines[2..*].join("\n"),
    "Prohibited regex interpolation (use the MONKEY-SEE-NO-EVAL pragma to\n"
    ~ "override this error but only if you're VERY sure your data contains no\n"
    ~ "injection attacks).",
    'message is the wrapped pragma advice';

is X::SecurityPolicy::Eval.new.message,
    "EVAL is a very dangerous function!!! (use the MONKEY-SEE-NO-EVAL pragma\n"
    ~ "to override this error but only if you're VERY sure your data contains\n"
    ~ "no injection attacks).",
    'default payload';
is X::SecurityPolicy::Eval.new(payload => 'x').message,
    "x (use the MONKEY-SEE-NO-EVAL pragma to override this error but only if\n"
    ~ "you're VERY sure your data contains no injection attacks).",
    'custom payload';
isa-ok X::SecurityPolicy::Eval.new, X::SecurityPolicy, 'is an X::SecurityPolicy';
