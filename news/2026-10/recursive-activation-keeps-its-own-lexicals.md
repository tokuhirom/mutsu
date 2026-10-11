# A recursive call keeps its own lexicals and attributes

A named sub declared in a routine or method, handed to `.map(&sub)` and
re-entering the routine, read the outer activation's lexicals (#12478):

```raku
class Node {
    has $.t; has @.kids;
    method tree {
        (<x>,).map(-> $name {
            my $type := $!t;
            sub show($x) { $x ~~ Node ?? "[" ~ $x.tree ~ "]" !! "$type:$x" }
            (self.kids.Slip, "end").map(&show).join(",")
        }).join
    }
}
```

mutsu printed `[OUT:end],...` where rakudo prints `[IN:end],...`, and the
inner node's `$!t` was overwritten with `OUT` afterwards. Three things went
wrong:

- The calling frame had been flattened, and a frame linked to the program
  scope (ADR-12529) skipped its callers straight into it.
- `my $type := $t` took `$t`'s cell from a caller's same-named binding.
- A bound `$!t` was published into the env, where every method called from
  there read it as its own invocant's attribute.

A flattened frame now records the program scope below it, and a linked frame
seeds its locals through the link. A bind takes a frame-local source's cell
only from that frame, and a bind of an attribute aliases the attribute's own
cell on its invocant. Methods of a class declared at program scope link their
frames past their callers, as program-scope subs already did.
