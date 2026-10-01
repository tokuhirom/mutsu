# Private-method checks for a lexical class declared inside a method

A call into a `my class` declared inside a method (`Inner2.new!Inner2::q`) was rejected even
when the nested class `trusts` the enclosing one, because the nested class was not registered
yet when the enclosing class's trust check ran. The check now reads the nested declaration's
`trusts` list from the AST. Conversely, a `self!p` in a nested class's method that names a
private method the nested class does not declare is now a compile-time
`X::Method::NotFound`, as in Rakudo.
