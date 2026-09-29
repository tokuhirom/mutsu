use v6;
use Test;

plan 9;

# A `{ … }` closure part of a heredoc / `qq` quote is its own Block call frame
# in Raku, exactly like one in a `"…"` string: `callframe(0)` inside it is the
# block, the enclosing routine is one level up.

sub heredoc() {
    qq:to/END/.chomp;
    {callframe(0).code.^name} {callframe(1).code.^name}
    END
}
is heredoc(), 'Block Sub', 'qq:to heredoc closure is a Block frame';

sub heredoc-named() {
    qq:to/END/.chomp;
    {callframe(1).code.name}
    END
}
is heredoc-named(), 'heredoc-named', 'callframe(1) in a heredoc closure is the routine';

sub qq-slash() { qq/{callframe(0).code.^name} {callframe(1).code.^name}/ }
is qq-slash(), 'Block Sub', 'qq/…/ closure is a Block frame';

sub qq-bang() { qq!{my $t = 1; $t + 1}! }
is qq-bang(), '2', 'multi-statement qq closure body still evaluates';

sub count-heredoc() {
    qq:to/END/.chomp;
    {$++}
    END
}
count-heredoc();
is count-heredoc(), '0', 'anon state in a heredoc closure restarts per call';

# The assignment forms of substitution take a thunk, not a closure Block: a
# placeholder in the RHS belongs to the enclosing block.
is-deeply [3, 4].map({ S{5} = $^a given "5" }).List, ("3", "4"),
    'S{…} = $^a placeholder belongs to the enclosing block';
is-deeply [3, 4].map({ my $s = "5"; $s ~~ s{5} = $^a; $s }).List, ("3", "4"),
    's{…} = $^a placeholder belongs to the enclosing block';

$_ = "abc";
s{(b)} = "[$0]";
is $_, 'a[b]c', 's{…} = EXPR sees the match captures';

is (S{b} = 'X' given 'abc'), 'aXc', 'S{…} = EXPR still substitutes';
