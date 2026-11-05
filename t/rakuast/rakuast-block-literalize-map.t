use Test;

plan 10;

sub lit(Str $src) {
    my $n = $src.AST.statements[0].expression;
    (try $n.literalize.raku) // "ERR: " ~ $!.^name;
}

# A block literalizes as the Map its statements' literal values build.
is lit('{ 1, 2 }'),            'Map.new(("1" => 2))', 'comma list';
is lit('{ 1, 2, 3, 4 }'),      'Map.new(("1" => 2,"3" => 4))', 'two pairs';
is lit('{ 1; 2 }'),            'Map.new(("1" => 2))', 'two statements';
is lit('{ "a", 5 }'),          'Map.new((:a(5)))', 'string key';
is lit('{ (1, 2) }'),          'Map.new(("1" => 2))', 'parenthesised list';
is lit('{ [1, 2] }'),          'Map.new(("1" => 2))', 'array is flattened';
is lit('{ (a => 1), (b => 2) }'), 'Map.new((:a(1),:b(2)))', 'pairs';
is lit('-> $x { 1, 2 }'),      'Map.new(("1" => 2))', 'pointy block';
is lit('{ 1 }'),               'ERR: X::Hash::Store::OddNumber', 'a dangling key dies';
is lit('{ 1, 2, 3 }'),         'ERR: X::Hash::Store::OddNumber', 'odd count dies';
