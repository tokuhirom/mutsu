# A module block's `my @a` / `my %h` keeps what its routines put in it

A `module`/`package`/`class` block's `my @users` lives in the package's static
store once the block has run, and the block's routines reach it only through
that store. Mutating it in place from one of those routines (`@users.push`,
`%h<k> = v`, `@a[$i]++`, `%h<k>:delete`, a nested element store) wrote into a
copy under the name in the routine's own env, so every other routine kept
seeing the empty container:

```raku
module M {
    my @users;
    my sub populate() { @users.push('root') }
    our sub get-it { populate(); @users.elems }   # was 0, now 1
}
```

The in-place write chokepoint and the element-store, element-increment and
`:delete` paths now resolve such a name to the store, the way reads already
did. Passing the aggregate as an argument also hands over the Array or Hash
itself rather than a Scalar holding it, so `first { ... }, @users`, `grep` and
a slurpy iterate its elements instead of taking it as one item — in a
`unit module` too. `System::Passwd`'s `t/Passwd.t` now passes (#10343).
