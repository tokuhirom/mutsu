# Complex component methods use handler rows

`Complex.re`, `im`, `reals` and `conj` now dispatch through their registered
method rows. The existing cascade path calls the same implementations, keeping
their behavior aligned while the remaining built-in methods migrate.
