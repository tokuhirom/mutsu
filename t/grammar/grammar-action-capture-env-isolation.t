use Test;

# An action method sees `$<name>` and `$0..` for ITS OWN match only: the
# bindings a parent action installed must be hidden while a child action runs,
# and put back afterwards. Both invocation paths implement that save/restore --
# the general walk and the childless-leaf fast path -- and this pins the
# behaviour for both, so the ceremony can be made cheaper without being made
# wrong (#7576).

plan 6;

grammar G {
    token TOP  { <outer> }
    token outer { $<tag>=(\w+) '=' (\w) <inner> }
    token inner { $<tag>=(\d+) ':' (\d+) }
}

my @seen;

class Actions {
    # The leaf: a childless match, taken by the leaf fast path.
    method inner($/) {
        @seen.push: "inner tag={$<tag>}";
        @seen.push: "inner pos0={$0}";
        make ~$/;
    }
    method outer($/) {
        # Re-read after the child action ran: the child's `$<tag>` / `$0` must
        # not have survived into this scope.
        @seen.push: "outer tag={$<tag>}";
        @seen.push: "outer pos0={$0}";
        make $<inner>.ast;
    }
    method TOP($/) { make $<outer>.ast }
}

my $m = G.parse('ab=x12:34', :actions(Actions.new));

ok $m.defined, 'parse succeeded';
is $m.ast, '12:34', 'leaf action made the inner text';

is @seen[0], 'inner tag=12', "leaf action sees its own \$<tag>";
is @seen[1], 'inner pos0=34', "leaf action sees its own \$0";
is @seen[2], 'outer tag=ab', "parent action's \$<tag> is restored after the child ran";
is @seen[3], 'outer pos0=x', "parent action's \$0 is restored after the child ran";
