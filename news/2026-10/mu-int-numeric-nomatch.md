# Mu.Int / Mu.Numeric of a plain object throw X::Multi::NoMatch

`.Int` and `.Numeric` on a defined instance of a class that declares neither now raise
`X::Multi::NoMatch` ("Cannot resolve caller Int(A:D: ); none of these signatures matches"),
as Rakudo does, instead of `X::Method::NotFound` naming `Any`. The type-object case is unchanged.
