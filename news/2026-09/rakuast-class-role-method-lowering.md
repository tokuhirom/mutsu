# RakuAST class, role, method and attribute lowering

`EVAL` now accepts `RakuAST::Class`, `RakuAST::Role`, `RakuAST::Method`, and
the attribute form of `RakuAST::VarDeclaration::Simple`. Before this change the
read direction rendered all four (Phase 2 slice 13, pinned by
`t/rakuast-class.t` and `t/rakuast-role.t`) while the write direction refused
the whole tree — `EVAL does not yet support lowering RakuAST::Class` — so the
declaration cluster was readable but not round-trippable.

## Change

`src/rakuast/lower.rs` gained four arms, each lowering into the existing
internal AST so the ordinary compiler and VM run the result. There is no second
execution path.

- `RakuAST::Class` -> `Stmt::ClassDecl`. Its `body` is a `Block` whose
  statements are the class body, lowered by the same statement dispatch, so
  methods and attributes come along for free.
- `RakuAST::Role` -> `Stmt::RoleDecl`. A role's body is a `RoleBody` wrapping
  the `Blockoid`, matching what the converter renders; a `Block` there is
  refused.
- `RakuAST::Method` -> `Stmt::MethodDecl`, the `Method` counterpart of
  `lower_sub`. It reads its return type through the same
  `signature.returns` / `Trait::Returns` / `Trait::Of` helper, so all three
  spellings the converter renders lower back, and the lowered return type is
  still enforced at run time.
- A `VarDeclaration::Simple` with `scope => "has"` -> `Stmt::HasDecl`, reading
  the twigil (`.` public / `!` private), the sigil, and an optional type. A
  typed attribute's implicit `BareWord(<TypeName>)` default — which the parser
  plants and the converter deliberately skips — is re-planted so the two
  directions stay symmetric.

Every richer form (inheritance, scope, reprs, traits, parameterised roles,
private/multi/submethods, attribute traits and `will build` defaults) is
already refused on the *read* side, so nothing that reaches these lowerers can
carry shape they would drop.

## A rendering bug fixed on the way

`method m() is raw { }` rendered as a plain `RakuAST::Method`, silently dropping
the trait. `is_raw` was added to `Stmt::MethodDecl` after the converter arm was
written, and the arm's "no traits" guard was never extended to it. It is now
refused (a coverage boundary) instead of rendered wrong, which is the rule the
rest of that arm already followed.

## Coverage

`t/rakuast-eval-class.t` (15 assertions) exercises an empty class, a class with
a method, a method with parameters, public/private/typed attributes and their
accessors, all three return-type spellings, run-time enforcement of a lowered
return type, and the role forms. Each program ends with its declaration, so the
`EVAL`'d value is the type object and the test calls into it from the outside —
referring to a user class by bare name *inside* the same program is a separate,
still-open read-direction gap (a user type name renders as a bareword). It is a
dual-oracle test: it passes verbatim under both mutsu and raku.
