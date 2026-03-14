use Test;

plan 8;

sub takes-defined-int(Int:D $x) { $x }
is takes-defined-int(42), 42, 'Int:D parameter accepts defined Int values';
throws-like { takes-defined-int(Int) },
    X::TypeCheck::Binding::Parameter,
    'Int:D parameter rejects Int type object';

sub takes-undefined-int(Int:U $x) { $x.gist }
is takes-undefined-int(Int), '(Int)', 'Int:U parameter accepts Int type object';
throws-like { takes-undefined-int(42) },
    X::TypeCheck::Binding::Parameter,
    'Int:U parameter rejects defined Int values';

sub takes-defined-str(Str:D $s) { $s }
is takes-defined-str('ok'), 'ok', 'Str:D parameter accepts defined Str values';
throws-like { takes-defined-str(Str) },
    X::TypeCheck::Binding::Parameter,
    'Str:D parameter rejects Str type object';

sub takes-undefined-str(Str:U $s) { $s.gist }
is takes-undefined-str(Str), '(Str)', 'Str:U parameter accepts Str type object';
throws-like { takes-undefined-str('nope') },
    X::TypeCheck::Binding::Parameter,
    'Str:U parameter rejects defined Str values';
