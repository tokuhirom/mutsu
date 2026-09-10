use v6;
use Test;

plan 8;

# A destructive s/// running as a smartmatch RHS must write back ONLY to the
# smartmatch LHS. The enclosing given/for topic-source scalar must not be
# touched (the topic is temporarily the LHS, not the given source).
# Regression: Text::CSV's `given $in { when Callable { $fragment ~~ s:i{^
# "row="} = ""; ... } }` clobbered $in with the substitution result.

{
    my $x = 42;
    my $s = "abc";
    given $x {
        $s ~~ s/a/X/;
        is $s, "Xbc", "smartmatch LHS updated by s///";
        is $x, 42, "given source scalar untouched by LHS s///";
        is $_, 42, "topic restored after LHS s///";
    }
}

# Assignment-replacement form under given.
{
    my $y = 43;
    my $t = "row=2-*";
    given $y {
        $t ~~ s:i{^ "row\=" } = "";
        is $t, "2-*", "assignment-form s{} = '' updated the LHS";
        is $y, 43, "given source scalar untouched by assignment-form s///";
    }
}

# A method's named param aliased as the given topic (the Text::CSV shape).
{
    my class Foo {
        method m (:$in!, Str :$fragment is copy) {
            given $in {
                when Callable {
                    $fragment ~~ s:i{^ "row\=" } = "";
                    return $in ~~ Callable ?? "callable:$fragment" !! "clobbered:$fragment";
                }
            }
        }
    }
    is Foo.new.m(in => sub { 42 }, fragment => "row=2-*"), "callable:2-*",
        "named param survives s/// on another var inside its given arm";
}

# `$_ ~~ s///` (LHS IS the topic) must still write through to the given source.
{
    my $z = "hello";
    given $z {
        $_ ~~ s/hello/world/;
        is $_, "world", "topic itself substituted";
    }
    is $z, "world", "given source updated when LHS is the topic";
}

done-testing;
