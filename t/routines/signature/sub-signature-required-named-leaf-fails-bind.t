use Test;

# A required named leaf of a sub-signature (`:at-keyw($type)!`) that the
# argument does not carry fails the bind itself (#14325), instead of leaving
# `$type` unbound for a later read to trip over.
plan 3;

sub f(:at-rule(%)! (Str:D :at-keyw($type)!, *%ast)) { $type }

throws-like { f(at-rule => {:x(1)}) }, X::AdHoc,
    message => /'at-keyw'/, 'a missing required named leaf fails the call';
is f(at-rule => {:at-keyw<media>}), 'media', 'the leaf binds when supplied';

sub g(:opt(%) (:$a, *%rest)) { $a // 'none' }
is g(opt => {:z(1)}), 'none', 'an optional named leaf may be absent';
