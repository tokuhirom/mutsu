use Test;

# When a block was required and not found, rakudo raises `X::Syntax::Missing`
# ("Missing block") rather than its catch-all `X::Syntax::Confused`. mutsu
# reported the generic "expected '{'" / "expected '}'" alternation, so
# `throws-like ..., X::Syntax::Missing` failed on the class even though the
# parse correctly rejected the source.
#
# roast/S04-statements/if.t and roast/S02-names/identifier.t assert this.

plan 5;

sub class-of($code) {
    try { EVAL $code };
    $!.^name;
}

is class-of('if 1; 2'), 'X::Syntax::Missing',
    'a conditional without a block is X::Syntax::Missing';
is class-of('sub foo-($x) { }'), 'X::Syntax::Missing',
    'a routine whose name ends the identifier early is X::Syntax::Missing';
is class-of('{my $x = 2;'), 'X::Syntax::Missing',
    'an unclosed block is X::Syntax::Missing';

try { EVAL 'if 1; 2' };
is $!.message.lines[0], 'Missing block', 'and its message says so';

# A block that *is* there still parses, so the new diagnosis is not reached on
# the happy path.
is EVAL('if 1 { 42 }'), 42, 'a conditional with a block still parses';
