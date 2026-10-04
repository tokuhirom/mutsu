# Anonymous types in a package display as `<anon|N>`

An anonymous `class`, `grammar` or `role` declared inside a package is registered under the
package-qualified internal marker (`Mx::__ANON_CLASS_2__`), which the display routine no longer
recognised, so `.^name` printed the marker. The display now looks at the last `::` segment, so the
enclosing package never leaks into the name of a type that has none (#11669).
