# "Did you mean" offers a user class's own methods

`C.new.alphe` on a class with `method alpha` now ends `Did you mean any of these: 'alpha', 'alpha2'?`
as rakudo's `X::Method::NotFound` does. The tip's candidates were the built-in method catalog only;
they now include the methods a user class declares along its MRO and its composed roles, and its
public attribute accessors (never a private method). A type the catalog does not list (a user class,
`Any`, `Mu`) was offered no `Any`/`Mu` names at all (`Any.say2` had no tip); it now gets them. Found by
the raku-doc differential sweep (`Language/grammar_tutorial.rakudoc`, `Type/Code.rakudoc`). A user
grammar's `Grammar`/`Match` names (`G.TOP` suggests `to`) are not offered yet.
