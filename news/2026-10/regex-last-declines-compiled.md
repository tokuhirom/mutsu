# The compiled regex engine takes the last declined pattern shapes

Five pattern shapes used to fall back to the tree walk. They now compile. Each is checked against
rakudo:

- a name on a separated quantifier's own token (`<alpha>+ % ','`);
- a code count with a separator (`<h> ** { $*MAX } % ':'`);
- a code count over a body that can match empty;
- a backreference inside a `&` conjunction branch;
- a backreference or code inside a `~` goal.

The conjunction and goal cases now see the enclosing captures, as rakudo's single cursor does.
Together with the `||` ratchet fix, no pattern in `t/grammar`, `t/regex` or `t/modules` declines
to the walk any more. That is ADR-0135's criterion for deleting the walk; the remaining walk uses
are bridges and context checks, tracked in #10255.
