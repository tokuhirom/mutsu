# Keep Array element cells in counted head and tail

`Array.head(n)` and `Array.tail(n)` now yield the original element containers.
Writing to the loop topic of either result updates the source array, including
elements that were holes. Immutable Lists retain their existing behavior.
