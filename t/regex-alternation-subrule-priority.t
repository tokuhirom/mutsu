use Test;

# Regression: in `||`/`|` alternation, when two subrule alternatives match the
# SAME span and both reach the surrounding anchor, the FIRST (highest-priority)
# alternative must win. A bug in the full-match selector inverted the priority of
# equal-rank ties (`.rev().find(...)`), so the LAST alternative won instead.

plan 6;

grammar Ordered {
    regex a { \w+ }
    regex b { \w+ }
    regex TOP { ^ [ <a> || <b> ] $ }
}
my $m = Ordered.parse('hello');
ok $m<a>.defined,  '|| picks the FIRST alternative (a is set)';
nok $m<b>.defined, '|| does not pick the second alternative (b unset)';

grammar OrderedSwapped {
    regex a { \w+ }
    regex b { \w+ }
    regex TOP { ^ [ <b> || <a> ] $ }   # b first now
}
my $m2 = OrderedSwapped.parse('hello');
ok $m2<b>.defined, 'priority follows written order, not subrule name';

# A real backtracking case: <description> (\N*) must give back chars so the
# rest of branch 1 can match, and branch 1 (written first) must still win.
grammar Inventory {
    regex color { \S+ }
    regex description { \N* }
    regex TOP { ^ [ <description> \s+ '(' \s* <color> \s* ')' || <color> \s+ <description> ] $ }
}
my $r = Inventory.parse('This is a description (red)');
is ~$r<description>, 'This is a description', 'branch-1 description via subrule backtracking';
is ~$r<color>, 'red', 'branch-1 color captured';

# LTM (|) with equal-length subrule matches: first alternative wins the tie.
grammar Ltm {
    regex a { \w+ }
    regex b { \w+ }
    regex TOP { ^ [ <a> | <b> ] $ }
}
ok Ltm.parse('world')<a>.defined, '| (LTM) equal-length tie keeps first alternative';
