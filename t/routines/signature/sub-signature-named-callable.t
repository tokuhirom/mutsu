use Test;

plan 2;

sub g(% (:&run!)) { run() }
is g({ run => { 2 } }), 2,
    'a required callable in a routine hash sub-signature binds by its key';

sub h(% (:$x!, :&run!)) { "$x " ~ run() }
is h({ x => 3, run => { 4 } }), '3 4',
    'callable and scalar leaves both bind in a routine hash sub-signature';
