# A module in the String::Utils shape: nothing carries an `is export` trait,
# and the whole export set is computed at load time by `sub EXPORT` out of the
# compunit's own unit scope. `private-helper` is deliberately withheld from the
# returned Map, so it exercises the one place the parse-time approximation is a
# superset of the real import set.

my sub private-helper($x) { $x ~ "!" }

my sub first-word(*@words) { @words[0] }

my sub joined(@words) { @words.join("-") }

my sub EXPORT(*@names) {
    Map.new: UNIT::.grep: {
        .key.starts-with('&')
          && !(.key eq '&EXPORT' | '&private-helper')
    }
}
