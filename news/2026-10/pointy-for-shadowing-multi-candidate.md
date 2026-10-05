# Pointy `for` parameters shadow sibling multi captures

Pointy `for` parameters now take precedence over a same-named free-variable
alias created for a sibling multi candidate. This preserves the loop variable
binding when routine-local multi candidates share captured names.
