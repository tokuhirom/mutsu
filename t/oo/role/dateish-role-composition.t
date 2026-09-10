use Test;

plan 10;

# Dateish is a real composable role in Raku -- both Date and DateTime
# themselves `does Dateish`, and a user class can `does Dateish` too (e.g.
# TOML::Thumb's `Time::Local`, which composes Dateish and supplies its own
# state plus a private `!formatter` for stringification).

lives-ok {
    EVAL q:to/RAKU/;
        class TimeOfDayProbe does Dateish {
            has $.hour;
            has $.min;
            has $.sec;

            method !formatter() {
                sprintf '%02d:%02d:%02d', $!hour, $!min, $!sec;
            }

            multi method new($hour, $min, $sec) { self.bless(:$hour :$min :$sec) }
        }
    RAKU
}, 'class Foo does Dateish { ... } is accepted (Dateish is a composable role)';

class TimeOfDay does Dateish {
    has $.hour;
    has $.min;
    has $.sec;

    method !formatter() {
        sprintf '%02d:%02d:%02d', $!hour, $!min, $!sec;
    }

    multi method new($hour, $min, $sec) { self.bless(:$hour :$min :$sec) }
}

my $t = TimeOfDay.new(1, 2, 3);
ok $t.does(Dateish), 'the instance does Dateish';
ok $t ~~ Dateish, 'smartmatch against Dateish sees the composed role';

# .Str/.gist/~/interpolation all resolve to the role's default Str, which in
# turn calls the composing class's own private !formatter -- mirroring how
# Date/DateTime privately implement !formatter themselves.
is $t.Str, '01:02:03', '.Str calls the private !formatter';
is $t.gist, '01:02:03', '.gist calls the private !formatter';
is ~$t, '01:02:03', 'prefix:<~> calls the private !formatter';
is "$t", '01:02:03', 'string interpolation calls the private !formatter';

# Date and DateTime still both compose Dateish themselves.
ok Date.new('2020-01-01').does(Dateish), 'Date does Dateish';
ok DateTime.new(:year<2020>).does(Dateish), 'DateTime does Dateish';

# A plain class that does NOT compose Dateish keeps its ordinary default Str
# (unaffected by the Dateish-specific fallback) -- it must not be routed
# through the (nonexistent) private !formatter.
class PlainNonDateish { has $.x; }
ok PlainNonDateish.new(x => 5).Str.starts-with('PlainNonDateish'),
    'a class that does not compose Dateish is unaffected';
