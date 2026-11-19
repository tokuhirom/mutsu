# A role attribute's `.package` is the role's generic `$?CLASS`

```raku
role R { has $.y }
say R.^attributes[0].package.HOW ~~ Metamodel::GenericHOW;   # was False, now True
```

Before a role is composed, the attributes it declares belong to the role's
generic `$?CLASS` placeholder, a `GenericHOW` type object, not to the role
group. AttrX::Mooish relies on this to tell a role attribute from a class
attribute when it looks up the composed copy. mutsu answered the role itself,
so that branch never ran. An uncomposed role's attribute now reports
`$?CLASS` as its package, with a `GenericHOW` metaobject; its copy in a class
still reports the class (#18215).
