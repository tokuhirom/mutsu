# A missing element of a coercion-typed hash or array is the target type object

`my P() %h; %h<k> .= new: ...` died with "No such method 'new' for invocant of type 'P()'": the default of a missing element carried the coercion spelling `P()` as its type name. It is now the nominal target (`P`), as for `my P() $x`, so `.new` reaches the class. CSS::Stylesheet's `CSS::AtPageRule` (`has CSS::Properties() %.margin-box`) needed it; its `t/at-page.t` now matches rakudo.
