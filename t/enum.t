use Test;
plan 35;

# Basic angle-bracket enum declaration
enum Day <Sun Mon Tue Wed Thu Fri Sat>;

# Qualified access
is Day::Sun.key, "Sun", "qualified access Day::Sun key";
is +Day::Sun, 0, "Day::Sun numeric value is 0";
is +Day::Mon, 1, "Day::Mon numeric value is 1";
is +Day::Sat, 6, "Day::Sat numeric value is 6";

# Short-name access
is Sun.key, "Sun", "short-name Sun resolves to enum";
is +Mon, 1, "short-name Mon numeric coercion";

# String coercion
is ~Sun, "Sun", "string coercion with prefix ~";
is ~Day::Fri, "Fri", "string coercion qualified";

# Numeric coercion
is 0 + Day::Sun, 0, "0 + Day::Sun is 0";
is 0 + Day::Wed, 3, "0 + Day::Wed is 3";

# .WHAT
is Day::Sun.WHAT, "(Day)", ".WHAT returns enum type name";

# .raku
is Day::Mon.raku, "Day::Mon", ".raku returns qualified name";

# .gist
is Day::Tue.gist, "Tue", ".gist returns key name";

# .Int / .Numeric
is Day::Thu.Int, 4, ".Int returns integer value";
is Day::Fri.Numeric, 5, ".Numeric returns integer value";

# .key / .value
is Day::Wed.key, "Wed", ".key returns key string";
is Day::Wed.value, 3, ".value returns integer value";

# .kv
is Day::Mon.kv.elems, 2, ".kv returns 2-element list";

# .pair
is Day::Tue.pair.key, "Tue", ".pair returns Pair with key";
is Day::Tue.pair.value, 2, ".pair returns Pair with value";

# .pred / .succ
is Day::Mon.pred.key, "Sun", ".pred goes to previous variant";
is Day::Mon.succ.key, "Tue", ".succ goes to next variant";

# === identity
ok Day::Mon === Day::Mon, "enum identity with ===";

# Explicit values with fat arrow
enum roman (i => 1, v => 5, x => 10, l => 50, c => 100);
is +roman::i, 1, "explicit value i => 1";
is +roman::v, 5, "explicit value v => 5";
is +roman::c, 100, "explicit value c => 100";

# Auto-increment after explicit value
enum mixed (a => 10, b, c);
is +mixed::a, 10, "explicit start at 10";
is +mixed::b, 11, "auto-increment to 11";
is +mixed::c, 12, "auto-increment to 12";

# .enums type-level method
is Day.enums.elems, 7, "Day.enums returns hash with 7 elements";

# Parenthesized enum variants can use colonpair and quoted-string entries.
enum day (:Sun(1), 'Mon', 'Tue', 'Wed');
is day(Tue), day(3), "day(Tue) same as day(3)";
my $today = "Today" but day(Tue);
ok $today.day ~~ day, "day(Tue).day is a day";
ok $today.day == Tue, "day(Tue) == Tue";
lives-ok { day($today) }, "day(\$today) lives";
ok $today.Tue, "day(Tue).Tue";
