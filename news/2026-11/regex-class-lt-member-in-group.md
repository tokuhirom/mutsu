# `<` as a character-class member inside a regex group

`/(<-[<]>*)/` died with "Unmatched ( in regex": the group-body scanner opened a
new `<...>` frame at the `<` between the class's brackets and never saw the
group's closing `)`. A `<` inside the brackets of a character class is a
member now. Webraku's `t/02-server.t` (which captures `(<-[<]>*)`) matches
rakudo, as do its other two test files.
