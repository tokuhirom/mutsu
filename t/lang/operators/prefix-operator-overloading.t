use Test;

plan 5;

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

{
    sub prefix:<@> ($thing) { return "AT$thing"; }
    my @values = [1, 2, 3];
    is-deeply @values, [1, 2, 3], "a prefix:<@> does not shadow an adjacent array variable";
}
