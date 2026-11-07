# EVAL's undeclared-variable scan: statement-form phasers and explicit invocants

The `EVAL` undeclared-variable scan rejected two valid programs: a variable
declared by a statement-form phaser (`NEXT (state $best) max= $score; LAST say $best`)
and the explicit invocant of a method declared in a nested block of a class body
(`method instance ($class: |c) { $class.new }`, whose capture marker dropped the
invocant's name). Both now scan like the parsed source. Three more `t/` files join
the `EVAL(slurp($file).AST)` ratchet (#16356).
