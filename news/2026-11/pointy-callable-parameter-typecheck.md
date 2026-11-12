# Pointy code parameters check Callable values

Control blocks with an `&` parameter now reject non-Callable values when binding
their topic, matching ordinary routine parameter binding. This covers `given`,
`with`, and `for` pointy blocks.
