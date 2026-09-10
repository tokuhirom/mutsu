use Test;

plan 4;

my @source = <old dog jumpso oover the foxo>;

my @all_parts;
Supply.from-list(@source).split("o").tap(-> $v { @all_parts.push($v) });
is-deeply @all_parts, ["", "ldd", "gjumps", "", "", "verthef", "x", ""],
  "Supply.split with string needle keeps empty parts";

my @limited_nonempty;
Supply.from-list(@source).split(/o/, 3, :skip-empty).tap(-> $v { @limited_nonempty.push($v) });
is-deeply @limited_nonempty, ["ldd", "gjumps", "verthef"],
  "Supply.split with regex needle, limit and :skip-empty";

my @zero_limit;
Supply.from-list(@source).split("o", 0).tap(-> $v { @zero_limit.push($v) });
is-deeply @zero_limit, [], "Supply.split with zero limit returns empty supply";

my @nonempty;
Supply.from-list(@source).split("o", :skip-empty).tap(-> $v { @nonempty.push($v) });
is-deeply @nonempty, ["ldd", "gjumps", "verthef", "x"],
  "Supply.split with :skip-empty removes empty parts";
