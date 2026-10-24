use v6;
use Test;

plan 10;

# An error raised by an object's own `Bool` method, run to test it for truth,
# unwinds the statement that tested it: the rest of the statement does not run.
class D { method Bool { die "boom" } }
my $d = D.new;

my @ran;
try { @ran.push("if"); my $x = ($d ?? "t" !! "f"); @ran.push("after-if"); CATCH { default { @ran.push("caught: " ~ .message) } } }
is @ran.join("|"), "if|caught: boom", "?? : the die unwinds, the rest of the block is skipped";

@ran = ();
try { my $x = ($d && 1); @ran.push("after-&&"); CATCH { default { @ran.push("caught: " ~ .message) } } }
is @ran.join("|"), "caught: boom", "&&";

@ran = ();
try { my $x = !$d; @ran.push("after-!"); CATCH { default { @ran.push("caught: " ~ .message) } } }
is @ran.join("|"), "caught: boom", "prefix !";

@ran = ();
try { my $x = ?$d; @ran.push("after-?"); CATCH { default { @ran.push("caught: " ~ .message) } } }
is @ran.join("|"), "caught: boom", "prefix ?";

@ran = ();
try { my $x = so $d; @ran.push("after-so"); CATCH { default { @ran.push("caught: " ~ .message) } } }
is @ran.join("|"), "caught: boom", "so";

@ran = ();
try { my $x = ($d || 1); @ran.push("after-||"); CATCH { default { @ran.push("caught: " ~ .message) } } }
is @ran.join("|"), "caught: boom", "||";

@ran = ();
try { if $d { @ran.push("then") } else { @ran.push("else") }; @ran.push("after"); CATCH { default { @ran.push("caught: " ~ .message) } } }
is @ran.join("|"), "caught: boom", "an if condition";

# A `Bool` that does not die is unaffected.
class Yes { method Bool { True } }
class No  { method Bool { False } }
is (Yes.new && "y"), "y", "a true Bool";
is (No.new || "n"), "n", "a false Bool";
is (!No.new), True, "! of a false Bool";
