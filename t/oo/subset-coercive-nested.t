use Test;

plan 3;

subtest 'coercive subset smartmatch and assignment' => {
    plan 5;

    subset OfCoercion of Int();
    my OfCoercion $v = 1;
    $v = "42";
    isa-ok $v, Int, 'typed variable keeps coerced target type';
    is $v, 42, 'typed variable assignment coerces value';

    subset NumStr of Num(Str);
    ok Str ~~ NumStr, 'source type object matches coercive subset';
    ok Num ~~ NumStr, 'target type object matches coercive subset';
    ok "42" ~~ NumStr, 'value matches coercive subset';
};

subtest 'definite coercive subset rules' => {
    plan 3;

    subset OfDefCoerce of Str:D(Rat);
    throws-like q<my OfDefCoerce $v>,
        X::Syntax::Variable::MissingInitializer,
        'definite coercive subset requires initializer';

    my sub bar(OfDefCoerce $s) { $s }
    is bar(3.14), "3.14", 'coercion into definite subset works';

    my class BadOne {
        method Str { Str }
    }
    throws-like { bar(BadOne.new) },
        X::TypeCheck::Binding::Parameter,
        'invalid coercion to type object throws parameter type error';
};

subtest 'nested coercers and subtest isolation' => {
    plan 3;

    eval-lives-ok q:to/SNIPPET/,
                    sub foo(Int() $x) { $x }
                    SNIPPET
                'first declaration compiles';
    eval-lives-ok q:to/SNIPPET/,
                    sub foo(Str:D $s) { $s }
                    SNIPPET
                'second declaration compiles in a clean eval context';

    class Source {
        method Num { pi }
        method Rat { 3.1415926 }
        method Str { die "Cannot coerce into Str" }
    }
    my Str(Num(Source)) $coerced;
    throws-like { $coerced = 42 },
        X::TypeCheck::Assignment,
        'nested coercive assignment rejects unacceptable value';
};
