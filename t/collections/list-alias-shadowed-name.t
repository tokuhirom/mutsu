use v6;
use Test;

plan 8;

# List-element container capture (`($a, $b) = ...` result lists, `\($a)`
# captures, `k => $v` pairs) must box the slot of the binding that is IN
# SCOPE at the site — not whichever `code.locals` slot happens to share the
# name. A same-named `my` in a sibling/inner block (a shadow slot) used to be
# picked by a by-name search, poisoning the variable's env entry with a cell
# holding the shadow slot's stale value; the next by-name read then adopted
# that cell (the CSV::Table comment-strip TWEAK-loop heisenbug,
# todo/deep/csv-table-comment-strip-loop-var-state-sync.md).

# Shadow block BEFORE the multi-assign, all inside an enclosing block.
{
    my $z = "a";
    my $w;
    { my $z = "shadow"; }
    ($z, $w) = "S", "C";
    is $z, "S", 'multi-assign target read back correctly (shadow before)';
    is $w, "C", 'second multi-assign target unaffected';
}

# Shadow block AFTER the multi-assign.
{
    my $x = "a";
    my $y;
    ($x, $y) = "S2", "C2";
    is $x, "S2", 'multi-assign target read back correctly (shadow after)';
    { my $x = "shadow"; }
}

# The CSV::Table shape: a sub-call multi-assign inside a for loop whose body
# also declares a same-named `my` in a conditional block.
sub my-strip($line is copy) {
    return "S:$line", "C";
}
my @lines;
for <a b c d> -> $line is copy {
    my $comment;
    ($line, $comment) = my-strip $line;
    @lines.push: $line;
    if @lines.elems == 1 {
        my $line = @lines.head;
        $line.defined;
    }
}
is-deeply @lines, ["S:a", "S:b", "S:c", "S:d"],
    'loop-var multi-assign survives a same-named shadow in the loop body';

# Plain (non-multi) assignment control case with a shadow.
my @c;
for <a b c> -> $line is copy {
    $line = "S:$line";
    @c.push: $line;
    { my $line = "shadow"; }
}
is-deeply @c, ["S:a", "S:b", "S:c"], 'plain assignment with shadow stays correct';

# The shadow's own value must remain intact (capture must not misdirect
# writes INTO the shadow slot either).
{
    my $p = "outer";
    my $seen;
    {
        my $p = "inner";
        my @l = ($p, 1);
        $seen = @l[0];
    }
    is $seen, "inner", 'inner-scope capture aliases the inner binding';
    is $p, "outer", 'outer binding untouched by inner capture';
}

# Pair-value capture (`k => $v`) with a same-named shadow nearby.
{
    my $v = 10;
    { my $v = 99; }
    my $pair = (k => $v);
    is $pair.value, 10, 'pair value captures the in-scope binding';
}

done-testing;
