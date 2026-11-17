# An `is rw` parameter refuses a Map element

An immutable `Map`'s element is a value, not a container, so passing
`$map<a>` straight to an `is rw` parameter now dies with `X::Parameter::RW`,
as on rakudo. Before, the argument path promoted the element to a cell
inside the Map and handed that cell over. The call then succeeded, and
promoting the element rewrote the shared Map in place. An `is raw`
parameter gets the element's value, and a Hash element is still written
through.
