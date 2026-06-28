use Test;

throws-like q[
    class GrammarUserClass {
        method bar { PostDeclaredGrammar.parse('OH HAI'); }
    }
    grammar PostDeclaredGrammar {
        rule TOP { .* }
    }
    GrammarUserClass.bar;
], X::Undeclared::Symbols, 'later grammar is not visible inside an earlier method body';

done-testing;
