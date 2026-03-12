use Test;

plan 7;

{
    sub dt(*%args) { DateTime.new(|{ year => 1984, %args }) }
    isa-ok dt.in-timezone(60*60).year, Int, 'bareword call target auto-invokes before method call';
}

{
    my $dt = DateTime.new(
        946684799,
        timezone  => -(5*60*60 + 55*60),
        formatter => { .day ~ '/' ~ .month ~ '/' ~ .year ~ ' ' ~ .second ~ 's' ~ .minute ~ 'm' ~ .hour ~ 'h' },
    );
    is ~$dt, '31/12/1999 59s4m18h', 'DateTime.new(Int) honors timezone and formatter';
}

{
    my $dt = DateTime.new('2009-12-31T22:33:44', formatter => -> $dt { ($dt.hour % 12) ~ 'ish' });
    is ~$dt, '10ish', 'DateTime.new(Str) honors formatter';
}

{
    my $then = now;
    is-deeply DateTime($then), DateTime.new($then), 'DateTime(Instant) coercer form works';
}

{
    my $now = DateTime.now;
    is-deeply $now.later([hours => 2, minutes => 30]), $now.later(hours => 2).later(minutes => 30),
        '.later accepts multiple units in a list argument';
}

{
    throws-like { DateTime.new(:2016year, 42) }, Exception,
        'DateTime.new rejects mixing positional with component named args';
}

{
    constant $dt1 = DateTime.new: :2017year, :11month, :15day, :18hour,
        :36minute, :second(17.25), :timezone(-5*3600);
    constant $dt2 = DateTime.new: :2015year, :12month, :25day, :3hour,
        :6minute,  :second(7.77),  :timezone(14*3600);
    constant $dur = Duration.new: 59826610.48;
    is-deeply $dt1 - $dt2, $dur, 'DateTime - DateTime returns precise Duration';
}
