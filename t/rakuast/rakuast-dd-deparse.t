use Test;

# dd of a RakuAST node prints its DEPARSE (with one trailing newline left
# off), not its .raku; inside a container it stays .raku (#18980).

plan 7;

sub dd-of(Str $code) {
    my $proc = run $*EXECUTABLE.absolute, '-e', $code, :err, :out;
    $proc.err.slurp(:close)
}

is dd-of(q{dd RakuAST::Name.from-identifier-parts('IO','Path')}), "IO::Path\n", 'a Name';
is dd-of(q{dd RakuAST::IntLiteral.new(42)}), "42\n", 'a literal';
is dd-of(q{dd RakuAST::Type::Coercion.new(base-type => RakuAST::Type::Simple.from-identifier("Int"), constraint => RakuAST::Type::Simple.from-identifier("Str"))}),
  "Int(Str)\n", 'a coercion type';
is dd-of(q{my $x = RakuAST::IntLiteral.new(42); dd $x}), "42\n", 'a variable gets no label';
is dd-of(q{dd RakuAST::StatementList.new(RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(1)))}),
  "1\n", 'one trailing newline of the DEPARSE goes';
is dd-of(q{dd RakuAST::IntLiteral}), "RakuAST::IntLiteral\n", 'a type object is not deparsed';
is dd-of(q{dd [RakuAST::IntLiteral.new(1)]}), "[RakuAST::IntLiteral.new(1)]\n", 'inside a container it is .raku';
