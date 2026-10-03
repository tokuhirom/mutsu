# Topic and loop bookkeeping leaves `Interpreter`

The sixth subsystem extraction under ADR-10779 (#10779) moved the 22 fields
that track `$_` and the constructs binding it into `TopicState`
(`src/runtime/topic_state.rs`):

- the topic's source variable and container, so a write to `$_` reaches what
  it aliases;
- the save stacks that `given`/`for` push and pop;
- `when`'s matched flag;
- the smartmatch right-hand-side context flags;
- `given`'s pointy-block captures;
- the per-loop local and parameter-name scopes.

Code reaches these fields as `self.topic_state.<field>`. A spawned thread
starts with all of them empty, as `clone_for_thread` already did field by
field; `TopicState::fork_for_thread` now says so in one place. The holder
field is allowed by `scripts/interp-fields.d/topic.txt`. `Interpreter` went
from 340 to 319 direct fields. This is a field move only: behaviour is
unchanged.
