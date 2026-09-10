use v6;
use Test;

# A leading-dot topic method call with a *quoted* method name — `."$m"()` and
# `.'m'()` — is `$_."$m"()`. mutsu used to reject the topic form (only the
# explicit-invocant `$obj."$m"()` worked), leaving the `.` unconsumed and
# failing with "Unexpected block in infix position".

plan 8;

# Interpolated (dynamic) method name on the topic.
{
    my $m = "uc";
    my $got;
    given "hi" { $got = ."$m"() }
    is $got, "HI", 'topic ."$m"() dynamic method name';
}

# Double-quoted static method name on the topic.
{
    my $got;
    given "hi" { $got = ."uc"() }
    is $got, "HI", 'topic ."uc"() static double-quoted name';
}

# Single-quoted static method name on the topic.
{
    my $got;
    given "hi" { $got = .'uc'() }
    is $got, "HI", "topic .'uc'() static single-quoted name";
}

# Inside a map block (the topic is the current element).
{
    my @a = <a b c>;
    is-deeply @a.map({ ."uc"() }).List, ("A", "B", "C").List,
        'topic quoted method call inside map block';
}

# Dynamic name inside map block.
{
    my $m = "uc";
    my @a = <x y>;
    is-deeply @a.map({ ."$m"() }).List, ("X", "Y").List,
        'topic dynamic method call inside map block';
}

# With arguments.
{
    my $m = "substr";
    my $got;
    given "hello" { $got = ."$m"(1, 3) }
    is $got, "ell", 'topic dynamic method call with arguments';
}

# The explicit-invocant form still works (regression guard).
{
    my $m = "uc";
    is "hi"."$m"(), "HI", 'explicit-invocant quoted method call still works';
}

# Inside a CATCH block, calling a quoted method on the exception topic.
{
    my $method = "message";
    my $seen;
    for 1 {
        CATCH {
            $seen = ."$method"();
            .resume;
        }
        die "boom";
    }
    is $seen, "boom", 'topic quoted method call inside CATCH block';
}
