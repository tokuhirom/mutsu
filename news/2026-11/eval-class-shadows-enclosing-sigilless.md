An EVAL class declaration now takes precedence over a same-named sigilless term
in the enclosing scope. This lets `my \Name = EVAL 'class Name { }; Name'` bind the
class object while preserving the enclosing term outside EVAL.
