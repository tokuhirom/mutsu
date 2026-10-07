# RakuAST: `also is Parent;`, `class ::Name` and `(while ...).m` keep their spelling

`.AST` now renders three constructs the way rakudo does (#12200). `also is Parent;` in a
class body is a `Statement::Also` with a `Trait::Is` instead of a trait of the class; the
parser already recorded it in `body_parents`, so the converter only splits it back out (it
leads the body, because the parser does not keep where the statement stood), and lowering
folds it into the parents again. `class ::Name` carries an internal `__leading_colons`
marker so the name renders with its leading `Name::Part::Empty`. A `while` written as a
term in parentheses is flagged `Stmt::While::is_bare_term`, so `(while ...).m` keeps the
`Circumfix::Parentheses(SemiList(...))` operand instead of becoming `StatementPrefix::Do`.
