# A `method` in a nested block of a class body is the class's method

A `method` declarator is has-scoped. Wherever it sits inside a class, role or
grammar body, it installs the method in that package. That includes a bare
block, a `do { }` block and an `if` branch. The method body still closes over
the block's lexicals. Data::Record relies on this to keep a helper `sub`
private to one method:

```raku
do { # hide this sub
    proto sub unrecord(Mu) is raw          {*}
    multi sub unrecord(Mu \value)          { value }
    method unrecord(::?CLASS:D: --> List:D) { @!record.map(&unrecord).List }
}
```

mutsu registered only the methods written directly in the body. A method in a
nested block became a package function that plain method calls fell back to,
so `.^methods`, `.can` and role requirements did not see it:
`use Data::Record::Tuple` died with "Method 'unrecord' must be implemented".

The parser now hoists such a declaration to the package body, so the normal
registration path installs it with its return type, private, submethod and
multi status and traits intact. Like in Rakudo, the method is installed even
when its block never runs. A marker left in the block builds an anonymous
method closure when the block runs. The closure is used only to capture the
block's variables. The marker also resolves the block's `sub`s and `proto`s
into `&name` bindings while the block is still live, because a `do { }` block
removes them from the routine registry when it exits. The hoisted method
receives this capture when it is installed. A role body runs again at every
composition, so each composition gives the composed methods its own capture
(`role R[$n] { do { my $q = $n; method m { $q } } }` answers per
parameterization). (#9525)
