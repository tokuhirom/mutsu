# Template::Mustache context precedence: resolved, and now pinned in `t/`

`todo/tickets/template-mustache-context-precedence-regression.md` recorded that
seven of `Template::Mustache`'s thirteen bundled upstream test files had
regressed — `01-basic` subtest 2 ("Context Precedence: Dotted names should be
resolved against former resolutions"), plus `06-logging`, `11-iterable`,
`12-inheritence`, `50-readme`, `91-specs` and `92-specs-file`. Its minimal
repro was:

```raku
use Template::Mustache;
say Template::Mustache.render(
    '{{#a}}{{b.c}}{{/a}}',
    { a => {b => {}}, b => {c => 'ERROR'} }
);
# raku: ''   mutsu: 'ERROR'
```

Both ends were re-measured for this entry rather than assumed:

- At the commit the ticket was written against (`5c169c530`) the repro renders
  `"ERROR"` and `01-basic` fails subtest 2, exactly as reported.
- On `main` at `76f74b104` the repro renders `""` and **all thirteen upstream
  files pass** (`01-basic` 10/10 … `92-specs-file` 10/10) against a release
  build. The regression was fixed somewhere in the ~80 pull requests that landed
  between those two commits; instrumenting the module at the old commit shows
  the failure was that entering a `{{#a}}` section did not push `a`'s value onto
  the resolution context stack (`@context` held 2 frames where raku held 3), so
  the dotted `b.c` fell through to the *outer* `b`.

## What is new here

The only thing that ever covered this was the **release-time** battery gate
(`scripts/battery-testsuite.sh`), which fetches the upstream suites and is not
run by ordinary CI — which is exactly why the regression survived unnoticed long
enough to need a ticket. `t/mustache-battery.t` closes that hole: like
`t/yaml-battery.t` and `t/openssl-battery.t`, it pins zero-config `use
Template::Mustache` resolution plus a ten-assertion smoke slice — variable
interpolation, HTML escaping vs `{{{ }}}`, list sections, falsy and inverted
sections, dotted names, partials via `:from`, and both directions of the context
precedence rule (the outer value must lose, and the inner value must win). Every
assertion also passes under `raku`.
