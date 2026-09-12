use Test;

# A parse failure inside a block body must be reported where the parser
# actually stopped, not where the enclosing declaration opened.
#
# The statement-list loop wrapped every failed `statement()` into its own
# "expected statement at line N" error and anchored that error at the start of
# the statement it had been trying to read. Because a block body is itself a
# statement list, every nesting level overwrote the position with its own
# start, and the outermost one won: a failure anywhere inside `class C { ... }`
# came out as "at line 1", pointing at the `class` line and naming no
# construct. Across the ecosystem parity ledger that made 99 distributions
# indistinguishable from each other (issue #7988) -- every one of them reported
# the first line of a module and an expectation set that varies with parser
# state rather than with the cause.
#
# `remaining_len` counts the UNCONSUMED tail, so the furthest position reached
# is the smallest one; the loop now keeps that instead of its own start.
# Every line asserted below is the line rakudo reports for the same source.

plan 4;

my $nested = q:to/CODE/;
class C {
    method a() { 1 }
    method b() { 2 }
    method c() { 3 }
    method d() {
        my $x = 1 1 1;
    }
}
CODE

try EVAL $nested;
is $!.^name, 'X::Syntax::Confused', 'an undiagnosed parse failure is still X::Syntax::Confused';
is $!.line, 6, 'the reported line is the failing routine, not the class opener on line 1';

# The same rule one level deeper: a sub nested inside a sub inside a class.
my $deeper = q:to/CODE/;
class D {
    method outer() {
        sub inner() {
            my $y = 2 2 2;
        }
    }
}
CODE

try EVAL $deeper;
is $!.line, 4, 'a failure two blocks deep is not attributed to the outermost one';

# A top-level failure keeps reporting its own line -- the change must not push
# the position past the statement that actually failed.
try EVAL "my \$a = 1;\nmy \$b = 3 3 3;\n";
is $!.line, 2, 'a top-level failure still reports its own line';
