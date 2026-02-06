use Test;
plan 10;

# Basic given/when
my $x = 42;
my $result = "";
given $x {
    when 1 { $result = "one"; }
    when 42 { $result = "forty-two"; }
    default { $result = "other"; }
}
is $result, "forty-two", 'basic given/when matches correct branch';

# default branch
given 99 {
    when 1 { $result = "one"; }
    when 2 { $result = "two"; }
    default { $result = "default"; }
}
is $result, "default", 'default branch when nothing matches';

# when with string matching
given "hello" {
    when "world" { $result = "world"; }
    when "hello" { $result = "hello"; }
    default { $result = "other"; }
}
is $result, "hello", 'given/when with string matching';

# first matching when wins (breakout)
$result = "";
given 5 {
    when 5 { $result = $result ~ "first"; }
    when 5 { $result = $result ~ "second"; }
}
is $result, "first", 'first matching when wins (breakout)';

# $_ is set inside given
my $topic = "";
given "topic" {
    $topic = $_;
}
is $topic, "topic", '$_ is set inside given block';

# $_ is restored after given
my $outer = "outer";
$_ = $outer;
given "inner" {
    when "inner" { $result = $_; }
}
is $_, "outer", '$_ is restored after given';

# given with integer comparison
given 10 {
    when 5 { $result = "five"; }
    when 10 { $result = "ten"; }
    when 15 { $result = "fifteen"; }
}
is $result, "ten", 'given/when with integer values';

# when without matching falls through
$result = "unchanged";
given 3 {
    when 1 { $result = "one"; }
    when 2 { $result = "two"; }
}
is $result, "unchanged", 'no match and no default leaves result unchanged';

# nested given
given "outer" {
    when "outer" {
        given "inner" {
            when "inner" { $result = "nested-match"; }
        }
    }
}
is $result, "nested-match", 'nested given/when works';

# given with True/False
given True {
    when True { $result = "true"; }
    when False { $result = "false"; }
}
is $result, "true", 'given/when with boolean values';
