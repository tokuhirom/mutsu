use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

# VM-native Stdout/Stderr output dispatch for $*OUT / $*ERR
# (print / put / say / printf / print-nl) — ③後段 emit_output PR-C. The output
# now resolves in the VM via its shared output-sink handle instead of bouncing
# through the interpreter. Captured from a subprocess via is_run so the real
# stdout/stderr bytes are checked.

plan 6;

is_run(
    '$*OUT.print("a"); $*OUT.print(42); $*OUT.say("hi"); $*OUT.put("yo");',
    "",
    { out => "a42hi\nyo\n", err => "", status => 0 },
    '$*OUT print / say / put write to stdout',
);

is_run(
    '$*OUT.say(1, 2, 3); $*OUT.printf("%d!", 7); $*OUT.say([1, 2, 3]);',
    "",
    { out => "123\n7![1 2 3]\n", err => "", status => 0 },
    '$*OUT multi-arg say / printf / list gist',
);

is_run(
    '$*ERR.say("e1"); $*ERR.print("e2\n");',
    "",
    { out => "", err => "e1\ne2\n", status => 0 },
    '$*ERR say / print write to stderr',
);

is_run(
    '$*OUT.print("x"); $*OUT.print-nl; $*OUT.print("y");',
    "",
    { out => "x\ny", err => "", status => 0 },
    '$*OUT.print-nl writes the output terminator',
);

is_run(
    'my $r = $*OUT.say("z"); print $r;',
    "",
    { out => "z\nTrue", err => "", status => 0 },
    '$*OUT.say returns True',
);

# a custom nl-out terminator is honored on stdout
is_run(
    '$*OUT.nl-out = "|"; $*OUT.say("p"); $*OUT.put("q");',
    "",
    { out => "p|q|", err => "", status => 0 },
    '$*OUT honors a custom nl-out',
);
