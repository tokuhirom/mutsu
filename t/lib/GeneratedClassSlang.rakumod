# Fixture for t/lang/parsing/slang-generated-class.t (#13370): a slang
# production that builds a class with the metamodel and installs it for the
# importing unit, as Qwiratry's `aggsub` does.
use v6.e.PREVIEW;

role Greeter { method greet() { "hello from " ~ self.^name } }

our role GeneratedClassGrammar {
    token routine-declarator:sym<defclass> {
        'defclass' <.end-keyword> <.ws> $<name>=<.identifier> <.ws> $<value>=[\d+]
    }
}

our role GeneratedClassActions {
    method routine-declarator:sym<defclass>(Mu $/) {
        my $name = ~$<name>;
        my $value = +~$<value>;
        my $class := Metamodel::ClassHOW.new_type(:$name);
        my @log;
        $class.^add_method('value', anon method value() { $value });
        $class.^add_method('bump', anon method bump() { @log.push($value); [+] @log });
        $class.^add_role(Greeter);
        $class.^compose;
        my $decl := RakuAST::VarDeclaration::Implicit::Constant.new(
            :$name, :value($class), :scope('our'));
        $*R.current-scope.merge-generated-lexical-declaration($decl, :resolver($*R));
        self.attach: $/, RakuAST::Term::Name.new(RakuAST::Name.from-identifier('Nil'));
    }
}

use Slangify GeneratedClassGrammar, GeneratedClassActions;
