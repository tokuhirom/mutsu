# `.VAR.name` of a missing element and of an itemized array's element

An element container's `.VAR.name` now matches rakudo in two more shapes
(#19080). An element that does not exist yet is an autovivifying container
named after its subscript: `my @b; @b[5].VAR.name` is `@b[5]` and
`my %h; %h<a>.VAR.name` is `%h{'a'}` (mutsu said `@b` and `%h`). An element of
an array a `$` variable holds lives in an anonymous container: `$x[0]` is
`element` and a missing `$x[7]` is `element[7]` (mutsu said `x`). A chained
subscript of a missing element (`@b[1][2].VAR.name`) answers `element[2]`
instead of dying. `dd` names its argument from the same descriptor, so
`dd @b[5]` prints `element of @b[5] = Any` as rakudo does.
