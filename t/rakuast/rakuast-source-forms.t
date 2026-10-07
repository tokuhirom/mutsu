use v6;
use Test;

# The source form of a construct survives into `.AST` text (#7564, S9):
# calls with and without parentheses, colonpairs, `^N`, topic calls, angle
# subscripts, mixins, attribute variables, private calls, nqp ops, type
# calls, statement prefixes, `()`, labels, `use v6.d`, parenthesized
# modifiers, postfix operands and `use newline`.
# Expected gists captured verbatim from Rakudo 2026.09; this file passes
# under BOTH mutsu and raku, so raku is the oracle.

plan 29;

is Q[sub f($a?) { }; f 1; f(1); f;].AST.gist, q:to/END/.chomp, 'call without parentheses';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Sub.new(
          name      => RakuAST::Name.from-identifier("f"),
          signature => RakuAST::Signature.new(
            parameters => (
              RakuAST::Parameter.new(
                type     => RakuAST::Type::Setting.new(
                  RakuAST::Name.from-identifier("Any")
                ),
                target   => RakuAST::ParameterTarget::Var.new(
                  name => "\$a"
                ),
                optional => True
              ),
            )
          ),
          body      => RakuAST::Blockoid.new(
            RakuAST::StatementList.new()
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Call::Name::WithoutParentheses.new(
          name => RakuAST::Name.from-identifier("f"),
          args => RakuAST::ArgList.new(
            RakuAST::IntLiteral.new(1)
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Call::Name.new(
          name => RakuAST::Name.from-identifier("f"),
          args => RakuAST::ArgList.new(
            RakuAST::IntLiteral.new(1)
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Call::Name::WithoutParentheses.new(
          name => RakuAST::Name.from-identifier("f")
        )
      )
    )
    END

is Q[sub f(*%_) { }; my $d; f(:a(1), :b, :!c, :$d);].AST.gist, q:to/END/.chomp, 'colonpair forms';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Sub.new(
          name      => RakuAST::Name.from-identifier("f"),
          signature => RakuAST::Signature.new(
            parameters => (
              RakuAST::Parameter.new(
                target => RakuAST::ParameterTarget::Var.new(
                  name => "\%_"
                ),
                slurpy => RakuAST::Parameter::Slurpy::Flattened
              ),
            )
          ),
          body      => RakuAST::Blockoid.new(
            RakuAST::StatementList.new()
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
          sigil       => "\$",
          desigilname => RakuAST::Name.from-identifier("d")
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Call::Name.new(
          name => RakuAST::Name.from-identifier("f"),
          args => RakuAST::ArgList.new(
            RakuAST::ColonPair::Value.new(
              key   => "a",
              value => RakuAST::Circumfix::Parentheses.new(
                RakuAST::SemiList.new(
                  RakuAST::Statement::Expression.new(
                    expression => RakuAST::IntLiteral.new(1)
                  )
                )
              )
            ),
            RakuAST::ColonPair::True.new("b"),
            RakuAST::ColonPair::False.new("c"),
            RakuAST::ColonPair::Variable.new(
              key   => "d",
              value => RakuAST::Var::Lexical.new("\$d")
            )
          )
        )
      )
    )
    END

is Q[^5; 0 ..^ 5;].AST.gist, q:to/END/.chomp, 'prefix caret and infix range';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPrefix.new(
          prefix  => RakuAST::Prefix.new("^"),
          operand => RakuAST::IntLiteral.new(5)
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
          left  => RakuAST::IntLiteral.new(0),
          infix => RakuAST::Infix.new("..^"),
          right => RakuAST::IntLiteral.new(5)
        )
      )
    )
    END

is Q[.say; $_.say;].AST.gist, q:to/END/.chomp, 'topic call';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Term::TopicCall.new(
          RakuAST::Call::Method.new(
            name => RakuAST::Name.from-identifier("say")
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPostfix.new(
          operand => RakuAST::Var::Lexical.new("\$_"),
          postfix => RakuAST::Call::Method.new(
            name => RakuAST::Name.from-identifier("say")
          )
        )
      )
    )
    END

is Q[my %h; %h<a>; %h<a b>; %h{'a'};].AST.gist, q:to/END/.chomp, 'angle subscripts';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
          sigil       => "\%",
          desigilname => RakuAST::Name.from-identifier("h")
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPostfix.new(
          operand => RakuAST::Var::Lexical.new("\%h"),
          postfix => RakuAST::Postcircumfix::LiteralHashIndex.new(
            index => RakuAST::QuotedString.new(
              processors => <words val>,
              segments   => (
                RakuAST::StrLiteral.new("a"),
              )
            )
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPostfix.new(
          operand => RakuAST::Var::Lexical.new("\%h"),
          postfix => RakuAST::Postcircumfix::LiteralHashIndex.new(
            index => RakuAST::QuotedString.new(
              processors => <words val>,
              segments   => (
                RakuAST::StrLiteral.new("a b"),
              )
            )
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPostfix.new(
          operand => RakuAST::Var::Lexical.new("\%h"),
          postfix => RakuAST::Postcircumfix::HashIndex.new(
            index => RakuAST::SemiList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::QuotedString.new(
                  segments   => (
                    RakuAST::StrLiteral.new("a"),
                  )
                )
              )
            )
          )
        )
      )
    )
    END

is Q[1 but 2; 1 does 2;].AST.gist, q:to/END/.chomp, 'mixin operators';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
          left  => RakuAST::IntLiteral.new(1),
          infix => RakuAST::Mixin.new("but"),
          right => RakuAST::IntLiteral.new(2)
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
          left  => RakuAST::IntLiteral.new(1),
          infix => RakuAST::Mixin.new("does"),
          right => RakuAST::IntLiteral.new(2)
        )
      )
    )
    END

is Q[class A { has $!x; has $.y; method m { $!x; $.y; self!p() }; method !p { } }].AST.gist, q:to/END/.chomp, 'attribute variables and private call';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Class.new(
          name => RakuAST::Name.from-identifier("A"),
          body => RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
              RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                  expression => RakuAST::VarDeclaration::Simple.new(
                    scope       => "has",
                    sigil       => "\$",
                    twigil      => "!",
                    desigilname => RakuAST::Name.from-identifier("x")
                  )
                ),
                RakuAST::Statement::Expression.new(
                  expression => RakuAST::VarDeclaration::Simple.new(
                    scope       => "has",
                    sigil       => "\$",
                    twigil      => ".",
                    desigilname => RakuAST::Name.from-identifier("y")
                  )
                ),
                RakuAST::Statement::Expression.new(
                  expression => RakuAST::Method.new(
                    name => RakuAST::Name.from-identifier("m"),
                    body => RakuAST::Blockoid.new(
                      RakuAST::StatementList.new(
                        RakuAST::Statement::Expression.new(
                          expression => RakuAST::Var::Attribute.new(
                            "\$!x"
                          )
                        ),
                        RakuAST::Statement::Expression.new(
                          expression => RakuAST::Var::Attribute::Public.new(
                            name => "\$.y"
                          )
                        ),
                        RakuAST::Statement::Expression.new(
                          expression => RakuAST::ApplyPostfix.new(
                            operand => RakuAST::Term::Self.new,
                            postfix => RakuAST::Call::PrivateMethod.new(
                              name => RakuAST::Name.from-identifier("p")
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                RakuAST::Statement::Expression.new(
                  expression => RakuAST::Method.new(
                    private => True,
                    name    => RakuAST::Name.from-identifier("p"),
                    body    => RakuAST::Blockoid.new(
                      RakuAST::StatementList.new()
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    END

is Q[use nqp; nqp::add_i(1, 2);].AST.gist, q:to/END/.chomp, 'nqp ops';
    RakuAST::StatementList.new(
      RakuAST::Pragma.new(
        name => "nqp"
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Nqp.new(
          "add_i",
          RakuAST::IntLiteral.new(1),
          RakuAST::IntLiteral.new(2)
        )
      )
    )
    END

is Q[Num(1); Array[Int]; Hash[Int](1);].AST.gist, q:to/END/.chomp, 'type calls';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPostfix.new(
          operand => RakuAST::Type::Simple.new(
            RakuAST::Name.from-identifier("Num")
          ),
          postfix => RakuAST::Call::Term.new(
            args => RakuAST::ArgList.new(
              RakuAST::IntLiteral.new(1)
            )
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Type::Parameterized.new(
          base-type => RakuAST::Type::Simple.new(
            RakuAST::Name.from-identifier("Array")
          ),
          args      => RakuAST::ArgList.new(
            RakuAST::Type::Simple.new(
              RakuAST::Name.from-identifier("Int")
            )
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPostfix.new(
          operand => RakuAST::Type::Parameterized.new(
            base-type => RakuAST::Type::Simple.new(
              RakuAST::Name.from-identifier("Hash")
            ),
            args      => RakuAST::ArgList.new(
              RakuAST::Type::Simple.new(
                RakuAST::Name.from-identifier("Int")
              )
            )
          ),
          postfix => RakuAST::Call::Term.new(
            args => RakuAST::ArgList.new(
              RakuAST::IntLiteral.new(1)
            )
          )
        )
      )
    )
    END

is Q[start { 1 }; quietly say 1; sink 1;].AST.gist, q:to/END/.chomp, 'statement prefixes';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::StatementPrefix::Start.new(
          RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
              RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                  expression => RakuAST::IntLiteral.new(1)
                )
              )
            )
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::StatementPrefix::Quietly.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Call::Name::WithoutParentheses.new(
              name => RakuAST::Name.from-identifier("say"),
              args => RakuAST::ArgList.new(
                RakuAST::IntLiteral.new(1)
              )
            )
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::StatementPrefix::Sink.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::IntLiteral.new(1)
          )
        )
      )
    )
    END

is Q[(); (()).elems;].AST.gist, q:to/END/.chomp, 'empty list';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Circumfix::Parentheses.new(
          RakuAST::SemiList.new()
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPostfix.new(
          operand => RakuAST::Circumfix::Parentheses.new(
            RakuAST::SemiList.new()
          ),
          postfix => RakuAST::Call::Method.new(
            name => RakuAST::Name.from-identifier("elems")
          )
        )
      )
    )
    END

is Q[L: for 1, 2 { }].AST.gist, q:to/END/.chomp, 'label';
    RakuAST::StatementList.new(
      RakuAST::Statement::For.new(
        labels => (
          RakuAST::Label.new("L"),
        ),
        mode   => "serial",
        source => RakuAST::ApplyListInfix.new(
          infix    => RakuAST::Infix.new(","),
          operands => (
            RakuAST::IntLiteral.new(1),
            RakuAST::IntLiteral.new(2),
          )
        ),
        body   => RakuAST::Block.new(
          implicit-topic     => True,
          required-topic     => True,
          may-have-signature => True,
          body               => RakuAST::Blockoid.new(
            RakuAST::StatementList.new()
          )
        )
      )
    )
    END

is Q[use v6.d;].AST.gist, q:to/END/.chomp, 'language version';
    RakuAST::StatementList.new(
      RakuAST::Statement::LanguageVersion.new(v6.d)
    )
    END

is Q[sub f($a) { }; (f(1) for ^2).join('|');].AST.gist, q:to/END/.chomp, 'parenthesized modifier operand';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Sub.new(
          name      => RakuAST::Name.from-identifier("f"),
          signature => RakuAST::Signature.new(
            parameters => (
              RakuAST::Parameter.new(
                type     => RakuAST::Type::Setting.new(
                  RakuAST::Name.from-identifier("Any")
                ),
                target   => RakuAST::ParameterTarget::Var.new(
                  name => "\$a"
                ),
                optional => False
              ),
            )
          ),
          body      => RakuAST::Blockoid.new(
            RakuAST::StatementList.new()
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPostfix.new(
          operand => RakuAST::Circumfix::Parentheses.new(
            RakuAST::SemiList.new(
              RakuAST::Statement::Expression.new(
                expression    => RakuAST::Call::Name.new(
                  name => RakuAST::Name.from-identifier("f"),
                  args => RakuAST::ArgList.new(
                    RakuAST::IntLiteral.new(1)
                  )
                ),
                loop-modifier => RakuAST::StatementModifier::For.new(
                  RakuAST::ApplyPrefix.new(
                    prefix  => RakuAST::Prefix.new("^"),
                    operand => RakuAST::IntLiteral.new(2)
                  )
                )
              )
            )
          ),
          postfix => RakuAST::Call::Method.new(
            name => RakuAST::Name.from-identifier("join"),
            args => RakuAST::ArgList.new(
              RakuAST::QuotedString.new(
                segments   => (
                  RakuAST::StrLiteral.new("|"),
                )
              )
            )
          )
        )
      )
    )
    END

is Q[(1 + 2).foo; ((1, 2)).elems; (1, 2).elems;].AST.gist, q:to/END/.chomp, 'postfix operand parentheses';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPostfix.new(
          operand => RakuAST::ApplyInfix.new(
            left  => RakuAST::IntLiteral.new(1),
            infix => RakuAST::Infix.new("+"),
            right => RakuAST::IntLiteral.new(2)
          ),
          postfix => RakuAST::Call::Method.new(
            name => RakuAST::Name.from-identifier("foo")
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPostfix.new(
          operand => RakuAST::Circumfix::Parentheses.new(
            RakuAST::SemiList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyListInfix.new(
                  infix    => RakuAST::Infix.new(","),
                  operands => (
                    RakuAST::IntLiteral.new(1),
                    RakuAST::IntLiteral.new(2),
                  )
                )
              )
            )
          ),
          postfix => RakuAST::Call::Method.new(
            name => RakuAST::Name.from-identifier("elems")
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPostfix.new(
          operand => RakuAST::ApplyListInfix.new(
            infix    => RakuAST::Infix.new(","),
            operands => (
              RakuAST::IntLiteral.new(1),
              RakuAST::IntLiteral.new(2),
            )
          ),
          postfix => RakuAST::Call::Method.new(
            name => RakuAST::Name.from-identifier("elems")
          )
        )
      )
    )
    END

is Q[* + 1; *.foo; (* + 1)(2);].AST.gist, q:to/END/.chomp, 'whatever code';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
          left  => RakuAST::Term::Whatever.new,
          infix => RakuAST::Infix.new("+"),
          right => RakuAST::IntLiteral.new(1)
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPostfix.new(
          operand => RakuAST::Term::Whatever.new,
          postfix => RakuAST::Call::Method.new(
            name => RakuAST::Name.from-identifier("foo")
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPostfix.new(
          operand => RakuAST::ApplyInfix.new(
            left  => RakuAST::Term::Whatever.new,
            infix => RakuAST::Infix.new("+"),
            right => RakuAST::IntLiteral.new(1)
          ),
          postfix => RakuAST::Call::Term.new(
            args => RakuAST::ArgList.new(
              RakuAST::IntLiteral.new(2)
            )
          )
        )
      )
    )
    END

is Q[use newline :crlf;].AST.gist, q:to/END/.chomp, 'newline pragma';
    RakuAST::StatementList.new(
      RakuAST::Statement::Use.new(
        module-name => RakuAST::Name.from-identifier("newline"),
        argument    => RakuAST::ColonPair::True.new("crlf")
      )
    )
    END

is Q[so 1; not 1;].AST.gist, q:to/END/.chomp, 'loose prefix words';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPrefix.new(
          prefix  => RakuAST::Prefix.new("so"),
          operand => RakuAST::IntLiteral.new(1)
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPrefix.new(
          prefix  => RakuAST::Prefix.new("not"),
          operand => RakuAST::IntLiteral.new(1)
        )
      )
    )
    END

is Q[1, 3 ... 9; 1 ^^ 2; (1) (|) (2);].AST.gist, q:to/END/.chomp, 'list associative infixes';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyListInfix.new(
          infix    => RakuAST::Infix.new("..."),
          operands => (
            RakuAST::ApplyListInfix.new(
              infix    => RakuAST::Infix.new(","),
              operands => (
                RakuAST::IntLiteral.new(1),
                RakuAST::IntLiteral.new(3),
              )
            ),
            RakuAST::IntLiteral.new(9),
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyListInfix.new(
          infix    => RakuAST::Infix.new("^^"),
          operands => (
            RakuAST::IntLiteral.new(1),
            RakuAST::IntLiteral.new(2),
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyListInfix.new(
          infix    => RakuAST::Infix.new("(|)"),
          operands => (
            RakuAST::Circumfix::Parentheses.new(
              RakuAST::SemiList.new(
                RakuAST::Statement::Expression.new(
                  expression => RakuAST::IntLiteral.new(1)
                )
              )
            ),
            RakuAST::Circumfix::Parentheses.new(
              RakuAST::SemiList.new(
                RakuAST::Statement::Expression.new(
                  expression => RakuAST::IntLiteral.new(2)
                )
              )
            ),
          )
        )
      )
    )
    END

is Q[my %h; %h<a> = 1; %h<a b> = 1, 2; %h<a> := 1; %h{'a'} = 1;].AST.gist, q:to/END/.chomp, 'angle subscript assignment and bind';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
          sigil       => "\%",
          desigilname => RakuAST::Name.from-identifier("h")
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPostfix.new(
          operand => RakuAST::Var::Lexical.new("\%h"),
          postfix => RakuAST::Postcircumfix::LiteralHashIndex.new(
            index    => RakuAST::QuotedString.new(
              processors => <words val>,
              segments   => (
                RakuAST::StrLiteral.new("a"),
              )
            ),
            assignee => RakuAST::IntLiteral.new(1)
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPostfix.new(
          operand => RakuAST::Var::Lexical.new("\%h"),
          postfix => RakuAST::Postcircumfix::LiteralHashIndex.new(
            index    => RakuAST::QuotedString.new(
              processors => <words val>,
              segments   => (
                RakuAST::StrLiteral.new("a b"),
              )
            ),
            assignee => RakuAST::ApplyListInfix.new(
              infix    => RakuAST::Infix.new(","),
              operands => (
                RakuAST::IntLiteral.new(1),
                RakuAST::IntLiteral.new(2),
              )
            )
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
          left  => RakuAST::ApplyPostfix.new(
            operand => RakuAST::Var::Lexical.new("\%h"),
            postfix => RakuAST::Postcircumfix::LiteralHashIndex.new(
              index => RakuAST::QuotedString.new(
                processors => <words val>,
                segments   => (
                  RakuAST::StrLiteral.new("a"),
                )
              )
            )
          ),
          infix => RakuAST::Infix.new(":="),
          right => RakuAST::IntLiteral.new(1)
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
          left  => RakuAST::ApplyPostfix.new(
            operand => RakuAST::Var::Lexical.new("\%h"),
            postfix => RakuAST::Postcircumfix::HashIndex.new(
              index => RakuAST::SemiList.new(
                RakuAST::Statement::Expression.new(
                  expression => RakuAST::QuotedString.new(
                    segments   => (
                      RakuAST::StrLiteral.new("a"),
                    )
                  )
                )
              )
            )
          ),
          infix => RakuAST::Assignment.new,
          right => RakuAST::IntLiteral.new(1)
        )
      )
    )
    END

is Q[$[1, 2]; ${ a => 1 }; $%(a => 1);].AST.gist, q:to/END/.chomp, 'item contextualizers';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Contextualizer::Item.new(
          RakuAST::Circumfix::ArrayComposer.new(
            RakuAST::SemiList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyListInfix.new(
                  infix    => RakuAST::Infix.new(","),
                  operands => (
                    RakuAST::IntLiteral.new(1),
                    RakuAST::IntLiteral.new(2),
                  )
                )
              )
            )
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Contextualizer::Item.new(
          RakuAST::Circumfix::HashComposer.new(
            RakuAST::FatArrow.new(
              key   => "a",
              value => RakuAST::IntLiteral.new(1)
            )
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Contextualizer::Item.new(
          RakuAST::Contextualizer::Hash.new(
            RakuAST::StatementSequence.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::FatArrow.new(
                  key   => "a",
                  value => RakuAST::IntLiteral.new(1)
                )
              )
            )
          )
        )
      )
    )
    END

is Q[lazy 1, 2; hyper 1, 2; race 1, 2;].AST.gist, q:to/END/.chomp, 'lazy hyper race prefixes';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::StatementPrefix::Lazy.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyListInfix.new(
              infix    => RakuAST::Infix.new(","),
              operands => (
                RakuAST::IntLiteral.new(1),
                RakuAST::IntLiteral.new(2),
              )
            )
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::StatementPrefix::Hyper.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyListInfix.new(
              infix    => RakuAST::Infix.new(","),
              operands => (
                RakuAST::IntLiteral.new(1),
                RakuAST::IntLiteral.new(2),
              )
            )
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::StatementPrefix::Race.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyListInfix.new(
              infix    => RakuAST::Infix.new(","),
              operands => (
                RakuAST::IntLiteral.new(1),
                RakuAST::IntLiteral.new(2),
              )
            )
          )
        )
      )
    )
    END

is Q[sub Foo::bar { }; Foo::bar(1); 1.Int::abs;].AST.gist, q:to/END/.chomp, 'qualified names';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Sub.new(
          name => RakuAST::Name.from-identifier-parts("Foo","bar"),
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new()
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Call::Name.new(
          name => RakuAST::Name.from-identifier-parts("Foo","bar"),
          args => RakuAST::ArgList.new(
            RakuAST::IntLiteral.new(1)
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPostfix.new(
          operand => RakuAST::IntLiteral.new(1),
          postfix => RakuAST::Call::Method.new(
            name => RakuAST::Name.from-identifier-parts("Int","abs")
          )
        )
      )
    )
    END

is Q[sub f(Int) { }].AST.gist, q:to/END/.chomp, 'type-only parameter';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Sub.new(
          name      => RakuAST::Name.from-identifier("f"),
          signature => RakuAST::Signature.new(
            parameters => (
              RakuAST::Parameter.new(
                type     => RakuAST::Type::Simple.new(
                  RakuAST::Name.from-identifier("Int")
                ),
                optional => False
              ),
            )
          ),
          body      => RakuAST::Blockoid.new(
            RakuAST::StatementList.new()
          )
        )
      )
    )
    END

is Q[my @a = []; [];].AST.gist, q:to/END/.chomp, 'empty array composer';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
          sigil       => "\@",
          desigilname => RakuAST::Name.from-identifier("a"),
          initializer => RakuAST::Initializer::Assign.new(
            RakuAST::Circumfix::ArrayComposer.new(
              RakuAST::SemiList.new()
            )
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Circumfix::ArrayComposer.new(
          RakuAST::SemiList.new()
        )
      )
    )
    END

is Q[{ $^a + $:b }].AST.gist, q:to/END/.chomp, 'placeholder variables';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Block.new(
          may-have-signature => True,
          body               => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyInfix.new(
                  left  => RakuAST::VarDeclaration::Placeholder::Positional.new(
                    "\$a"
                  ),
                  infix => RakuAST::Infix.new("+"),
                  right => RakuAST::VarDeclaration::Placeholder::Named.new(
                    "\$b"
                  )
                )
              )
            )
          )
        )
      )
    )
    END

is Q[use Test; is-deeply(1, 1, :a(1)); throws-like({ 1 }, X::OutOfRange, :message('x'));].AST.gist, q:to/END/.chomp, 'test call colonpairs';
    RakuAST::StatementList.new(
      RakuAST::Statement::Use.new(
        module-name => RakuAST::Name.from-identifier("Test")
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Call::Name.new(
          name => RakuAST::Name.from-identifier("is-deeply"),
          args => RakuAST::ArgList.new(
            RakuAST::IntLiteral.new(1),
            RakuAST::IntLiteral.new(1),
            RakuAST::ColonPair::Value.new(
              key   => "a",
              value => RakuAST::Circumfix::Parentheses.new(
                RakuAST::SemiList.new(
                  RakuAST::Statement::Expression.new(
                    expression => RakuAST::IntLiteral.new(1)
                  )
                )
              )
            )
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Call::Name.new(
          name => RakuAST::Name.from-identifier("throws-like"),
          args => RakuAST::ArgList.new(
            RakuAST::Block.new(
              may-have-signature => True,
              body               => RakuAST::Blockoid.new(
                RakuAST::StatementList.new(
                  RakuAST::Statement::Expression.new(
                    expression => RakuAST::IntLiteral.new(1)
                  )
                )
              )
            ),
            RakuAST::Type::Simple.new(
              RakuAST::Name.from-identifier-parts("X","OutOfRange")
            ),
            RakuAST::ColonPair::Value.new(
              key   => "message",
              value => RakuAST::Circumfix::Parentheses.new(
                RakuAST::SemiList.new(
                  RakuAST::Statement::Expression.new(
                    expression => RakuAST::QuotedString.new(
                      segments   => (
                        RakuAST::StrLiteral.new("x"),
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    END

is Q[.^name; .?foo; .Str;].AST.gist, q:to/END/.chomp, 'topic metamethod calls';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Term::TopicCall.new(
          RakuAST::Call::MetaMethod.new(
            name => "name"
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Term::TopicCall.new(
          RakuAST::Call::Method.new(
            name     => RakuAST::Name.from-identifier("foo"),
            dispatch => ".?"
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Term::TopicCall.new(
          RakuAST::Call::Method.new(
            name => RakuAST::Name.from-identifier("Str")
          )
        )
      )
    )
    END

is Q[sub f(*%_) { }; f(:a<x y>, :b[1, 2], :c(1)); f :a<x>;].AST.gist, q:to/END/.chomp, 'bracketed colonpair values';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Sub.new(
          name      => RakuAST::Name.from-identifier("f"),
          signature => RakuAST::Signature.new(
            parameters => (
              RakuAST::Parameter.new(
                target => RakuAST::ParameterTarget::Var.new(
                  name => "\%_"
                ),
                slurpy => RakuAST::Parameter::Slurpy::Flattened
              ),
            )
          ),
          body      => RakuAST::Blockoid.new(
            RakuAST::StatementList.new()
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Call::Name.new(
          name => RakuAST::Name.from-identifier("f"),
          args => RakuAST::ArgList.new(
            RakuAST::ColonPair::Value.new(
              key   => "a",
              value => RakuAST::QuotedString.new(
                processors => <words val>,
                segments   => (
                  RakuAST::StrLiteral.new("x y"),
                )
              )
            ),
            RakuAST::ColonPair::Value.new(
              key   => "b",
              value => RakuAST::Circumfix::ArrayComposer.new(
                RakuAST::SemiList.new(
                  RakuAST::Statement::Expression.new(
                    expression => RakuAST::ApplyListInfix.new(
                      infix    => RakuAST::Infix.new(","),
                      operands => (
                        RakuAST::IntLiteral.new(1),
                        RakuAST::IntLiteral.new(2),
                      )
                    )
                  )
                )
              )
            ),
            RakuAST::ColonPair::Value.new(
              key   => "c",
              value => RakuAST::Circumfix::Parentheses.new(
                RakuAST::SemiList.new(
                  RakuAST::Statement::Expression.new(
                    expression => RakuAST::IntLiteral.new(1)
                  )
                )
              )
            )
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Call::Name::WithoutParentheses.new(
          name => RakuAST::Name.from-identifier("f"),
          args => RakuAST::ArgList.new(
            RakuAST::ColonPair::Value.new(
              key   => "a",
              value => RakuAST::QuotedString.new(
                processors => <words val>,
                segments   => (
                  RakuAST::StrLiteral.new("x"),
                )
              )
            )
          )
        )
      )
    )
    END
