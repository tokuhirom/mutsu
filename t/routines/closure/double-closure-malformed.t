use Test;

plan 2;

throws-like 'sub foo($a where {* < 5 or $a > 9}) { say $a }',
    X::Syntax::Malformed,
    :what{ .contains("closure") },
    "where block with a Whatever closure is malformed";

throws-like 'sub foo($a where {* < 5 and * > 9}) { say $a }',
    X::Syntax::Malformed,
    :what{ .contains("closure") },
    "multiple Whatever closures in where block are malformed";
