# A mixin reaches its roles' rows; RakuAST nodes and origins are opaque shapes

Two owner hooks of the method table are gone. A `Mixin` receiver now reaches,
ahead of its inner value's rows, the rows of the roles mixed into it that are
catalog owners, most recently applied first (`mixin_role_owners_of`), which
retires the wrapper-role-specific hook behind `X::Promise::Broken`,
`X::React::Died` and `X::Await::Died`'s `gist`. A RakuAST node is the opaque
`RakuAstNode` shape, and `RakuAST::Origin` / `RakuAST::Origin::Source`
instances are opaque shapes of their own, so `rakuast_owners_of` is gone too.
