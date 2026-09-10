use v6;
use Test;

plan 8;

# Raku's hash-iteration pairs are LIVE: a `%h{$p.key} = X` inside the loop body
# is observable through a later `$p.value` read, and a `$p.value = X` writes back
# to the hash. Surfaced while driving the real zef CLI: Zef::Config::parse-file
# chains `%config{k} = $node.value.subst(...)` over `for %config -> $node`, which
# only interpolates `$*HOME` correctly when `$node.value` reflects the prior
# subst write.

# 1. hash write is visible through a later $p.value read (the zef pattern)
{
    my %h = a => "orig";
    for %h -> $p {
        %h{$p.key} = "changed";
        is $p.value, "changed", 'a %h{key}=X write is visible through $p.value';
    }
}

# 2. writing $p.value updates the hash
{
    my %h = a => "orig";
    for %h -> $p {
        $p.value = "via-pair";
    }
    is %h<a>, "via-pair", '$p.value = X writes back to the hash';
}

# 3. chained substitution (the exact Zef::Config shape)
{
    my %config = StoreDir => '$*HOME/.zef/store';
    my $home = "/home/user";
    for %config -> $node {
        if $node.key.ends-with('Dir') {
            %config{$node.key} = $node.value.subst(/'$*HOME'/, $home, :g);
            %config{$node.key} = $node.value.subst(/'{time}'/, '0', :g);
        }
    }
    is %config<StoreDir>, "/home/user/.zef/store",
        'chained subst over a hash-iteration pair interpolates correctly';
}

# 4. $p.value participates in arithmetic (must deref, not stay a raw ref)
{
    my %h = a => 3, b => 4;
    my $sum = 0;
    for %h -> $p { $sum += $p.value; }
    is $sum, 7, '$p.value coerces to a number in arithmetic';
}

# 5. the whole pair still gists correctly
{
    my %h = a => 1;
    for %h -> $p { is $p.gist, "a => 1", 'the pair still gists with its value'; }
}

# 6. object-hash (typed) keys still iterate live
{
    my %o{Int} = 1 => "x";
    for %o -> $p {
        is $p.value, "x", 'a typed-key hash iterates live pairs too';
    }
}

# 7. topic form ($_) is live as well
{
    my %h = a => "orig";
    for %h {
        %h{.key} = "topic-changed";
        is .value, "topic-changed", 'the topic $_ pair is live too';
    }
}

# 8. a pair that escapes the loop keeps reading its value
{
    my %h = a => 5;
    my $escaped;
    for %h -> $p { $escaped = $p; }
    is $escaped.value, 5, 'an escaped pair still reads its value';
}
