# `.origin` of literal-keyed hash pairs and capture literals

Two node families now carry the ranges rakudo 2026.11 gives them:

- **Literal-keyed pairs in a hash composer** (`{"1" => 2}`, `{1 => 2, 'b' => 3}`):
  each pair's key, its `=>` and the pair itself are placed. The parser keeps
  the written key beside the value, not as a term of the unit, so the key is
  read back from the source just before the `=>` that precedes the value,
  whose range is known.
- **Capture literals** (`\(1, :a)`, `\()`): the `ArgList` covers the arguments,
  or is empty just inside the `(`.

(#15780)
