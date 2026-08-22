# A `CONTROL { when CX::Take {...} }` handler is never invoked for `take` inside `gather`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/phasers.rakudoc:401`).

## Repro

```raku
say elems gather {
    CONTROL {
        when CX::Warn { say "WARNING!!! $_"; .resume }
        when CX::Take { say "Don't take my stuff"; .resume }
        when CX::Done { say "Done"; .resume }
    }
    warn 'people take stuff here';
    take 'keys';
    done;
}
```

- `raku`:
  ```
  WARNING!!! people take stuff here
  Don't take my stuff
  Done
  0
  ```
- `mutsu` (`target/debug/mutsu`):
  ```
  WARNING!!! people take stuff here
  Done
  1
  ```

Verified directly with `raku -e` / `target/debug/mutsu -e` on this exact snippet. The
`CX::Warn` and `CX::Done` branches both fire correctly (mutsu prints "WARNING!!!" and
"Done"), but the `CX::Take` branch never fires — `take 'keys'` proceeds as a normal,
un-intercepted take (contributing 1 element to the gather), instead of being caught by
the `CONTROL` block, printing "Don't take my stuff", and being resumed as a no-op
(`.resume` on a caught `CX::Take` discards the taken value per the doc's semantics,
hence raku's final count of `0`).

`CX::Take` itself exists as a type in mutsu (`say CX::Take.^name` → `CX::Take`), so this
isn't a missing-type gap — it's that `take`'s control-flow signal isn't routed through
the same `CONTROL`-catchable control-exception machinery that `warn` and `done` already
use.

## Root cause hypothesis

Wherever mutsu implements `warn`/`done` as catchable control exceptions dispatched to an
enclosing `CONTROL` block (grep for `"CX::Warn"` / `"CX::Done"` handling in
`src/runtime/` or `src/vm/`), `take` doesn't raise the equivalent `CX::Take` control
exception through that same path — it's likely implemented as a direct
gather/take-buffer push with no control-exception signal at all, so there is nothing for
`CONTROL`'s `when CX::Take` to catch.

## Affected files (starting point)

- Wherever `warn`/`done` control-exception dispatch to `CONTROL` lives (grep for
  `"CX::Warn"`, `"CONTROL"` in `src/runtime/`, `src/vm/`) — compare against wherever
  `take` is implemented (grep for `"take"` handling in gather-related VM ops) to see why
  it doesn't raise a matching `CX::Take` control exception.
