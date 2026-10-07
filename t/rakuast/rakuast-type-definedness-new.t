use Test;

plan 10;

my $int = RakuAST::Type::Simple.new(RakuAST::Name.from-identifier("Int"));
my $str = RakuAST::Type::Simple.new(RakuAST::Name.from-identifier("Str"));

my $d = RakuAST::Type::Definedness.new(base-type => $int, definite => True);
is $d.raku, q:to/R/.chomp, 'Type::Definedness.new (definite) .raku';
RakuAST::Type::Definedness.new(
  base-type => RakuAST::Type::Simple.new(
    RakuAST::Name.from-identifier("Int")
  ),
  definite  => True
)
R
is $d.definite, True, '.definite';
is $d.DEPARSE, 'Int:D', 'DEPARSE of :D';
is RakuAST::Type::Definedness.new(base-type => $int, definite => False).DEPARSE,
    'Int:U', 'DEPARSE of :U';

my $any = RakuAST::Type::AnyDefinedness.new(base-type => $int);
is $any.raku, q:to/R/.chomp, 'Type::AnyDefinedness.new .raku';
RakuAST::Type::AnyDefinedness.new(
  base-type => RakuAST::Type::Simple.new(
    RakuAST::Name.from-identifier("Int")
  )
)
R
is $any.DEPARSE, 'Int:_', 'DEPARSE of :_';

my $co = RakuAST::Type::Coercion.new(base-type => $int, constraint => $str);
is $co.raku, q:to/R/.chomp, 'Type::Coercion.new .raku';
RakuAST::Type::Coercion.new(
  base-type  => RakuAST::Type::Simple.new(
    RakuAST::Name.from-identifier("Int")
  ),
  constraint => RakuAST::Type::Simple.new(
    RakuAST::Name.from-identifier("Str")
  )
)
R
is $co.DEPARSE, 'Int(Str)', 'DEPARSE of a coercion';
is RakuAST::Type::Coercion.new(base-type => $int).DEPARSE, 'Int()',
    'a coercion without constraint';
is EVAL($d).raku, 'Int:D', 'EVAL of a hand-built :D type';
