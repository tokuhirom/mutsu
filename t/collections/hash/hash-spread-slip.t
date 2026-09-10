use Test;

plan 3;

{
    sub collect(*%h) { %h }
    my %args = month => 10, day => 12;
    my %got = collect(|{ year => 1984, %args });
    is-deeply %got, { year => 1984, month => 10, day => 12 }, "hash literal spread slips into *%";
}

{
    class Probe {
        method take(*%h) { %h<year> ~ "-" ~ %h<month> ~ "-" ~ %h<day> }
    }

    my %args = month => 10, day => 12;
    is Probe.new.take(|{ year => 1984, %args }), "1984-10-12", "method call accepts slipped hash spread as named args";
}

{
    sub pair-count(*%h) { +%h.keys }
    my %args = :month(10), :day(12);
    is pair-count(|{ year => 1984, |%args }), 3, "mixed spread forms inside hash literal flatten correctly";
}
