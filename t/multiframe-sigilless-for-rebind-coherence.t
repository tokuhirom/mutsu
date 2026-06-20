use Test;

# Slice F (env<->locals coherence): an inner `for (...) -> $x, \value` that
# re-binds a sigilless loop variable sharing its name (and local slot) with an
# enclosing `\value` must not leave the outer binding clobbered after the loop.
# With the reverse env->locals pull disabled (single-store default), the
# multi-param restore must write the saved value through to the local slot too,
# otherwise the next outer iteration re-evaluates the loop's element list with a
# stale `value`. Regression: roast/S32-str/val.t (661 subtests).

plan 4;

# Minimal: outer `\value` read inside the list expression on each outer pass.
{
    my \value = 42;
    my @got;
    for 1..2 -> $i {
        for (
           "a$i",  value,
          "b$i", -value,
        ) -> $string, \value {
            @got.push("$string=" ~ value);
        }
    }
    is-deeply @got, ["a1=42", "b1=-42", "a2=42", "b2=-42"],
      "inner sigilless \\value rebind does not clobber outer \\value across outer iterations";
}

# Via a sub param `\value` (the val.t shape).
{
    sub probe(\value, @strings) {
        my @got;
        for @strings -> $string {
            for (
               "$string",  value,
              "-$string", -value,
            ) -> $string, \value {
                @got.push("$string=" ~ value);
            }
        }
        @got;
    }
    is-deeply probe(42, ["42", "4_2"]),
      ["42=42", "-42=-42", "4_2=42", "-4_2=-42"],
      "sub param \\value preserved across @strings iterations";
}

# Closure inside the inner loop reads the correct rebound value.
{
    my \value = 7;
    my @got;
    for 1..2 -> $i {
        for ("x", value, "y", value * 10) -> $s, \value {
            my $c = { value };
            @got.push($c());
        }
    }
    is-deeply @got, [7, 70, 7, 70], "closure capture reads rebound sigilless value";
}

# Three outer iterations to ensure it is not a one-shot fix.
{
    my \base = 100;
    my @got;
    for 1..3 -> $i {
        for ("p", base + $i, "q", base - $i) -> $s, \base {
            @got.push(base);
        }
    }
    is-deeply @got, [101, 99, 102, 98, 103, 97],
      "outer \\base re-read correctly across three iterations";
}
