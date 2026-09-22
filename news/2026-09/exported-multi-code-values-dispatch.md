# Exported multi code values dispatch through their captured candidates

An exported `proto`/`multi` routine captured as a code value inside its
defining module now dispatches through the captured candidates. Previously,
the call re-resolved the proto in the caller's package and died with
`Cannot resolve caller json-eqv(Hash:D)`.

The regression was found by re-running `CSS::Module::CSS3::Selectors` 0.0.6.
Its `t/00basic.t` file moved from dying after three assertions to reaching all
92 assertions; 84 pass under mutsu. The remaining eight assertions are
independent parser/regex findings recorded in [#9053](https://github.com/tokuhirom/mutsu/issues/9053)
and [#9054](https://github.com/tokuhirom/mutsu/issues/9054).
