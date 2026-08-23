use v6;
use Test;

plan 2;

proto greeting (Str \name --> Str) {*}
proto named (Str $name --> Str) {*}

is &greeting.signature.gist, '(Str \\name --> Str)',
    'proto signature preserves a sigilless parameter';
is &named.signature.gist, '(Str $name --> Str)',
    'proto signature preserves a named parameter';
