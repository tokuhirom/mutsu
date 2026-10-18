# Classes composing `X::Control` inherit `Exception`

`X::Control`'s built-in role-parent metadata now records its `Exception` parent.
Classes that compose the role therefore include `Exception` in their MRO and
inherit its methods through the ordinary role-composition path.
