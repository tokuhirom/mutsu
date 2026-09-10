use Test;

plan 4;

{
    sub prefix:<X> ($thing) { return "ROUGHLY$thing"; }
    is X "fish", "ROUGHLYfish", "word-like custom prefix operator";
}

{
    sub prefix:["\x[2213]"] ($thing) { return "AROUND$thing"; }
    is ∓ "fish", "AROUNDfish", "quoted custom prefix declaration unescapes operator name";
}

{
    sub prefix:<(+-)> ($thing) { return "ABOUT$thing"; }
    is EVAL(q[ (+-) "fish" ]), "ABOUTfish", "parenthesized custom prefix operator in EVAL";
}

{
    my sub prefix:<->($thing) { return "CROSS$thing"; }
    is -"fish", "CROSSfish", "custom prefix overrides built-in operator";
}
