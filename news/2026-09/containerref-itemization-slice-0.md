# Clarify why destructuring staging values are not element containers

The comments around the list-destructuring staging temporary now record the
measured Raku rule: it models the right-hand-side `List`, whose elements are
values rather than `Array` element containers. The staging-temp exclusion is
therefore independent of the later `ContainerRef` holder-itemization work in
ADR-0079, and is not an ambiguity workaround.
