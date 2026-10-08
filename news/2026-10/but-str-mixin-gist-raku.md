# A Str mixin now wins for `.gist` and `.raku`

`(5 but "x").gist` and `.raku` answered the inner number; Rakudo answers the mixed-in `Str`
(`x`), as mutsu already did for `.Str`. A `Str` mixed into any value now wins for `.gist`,
`.raku` and `.perl`. Non-`Str` mixins (`5 but 7`, `"a" but 5`) and the `IntStr.new(...)`
rendering of allomorphs are unchanged. Closes #12303.
