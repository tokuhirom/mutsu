# Fixture for t/modules/import-export/export-sub-selective-lexical-multi.t: the shape of
# lizmat's Identity::Utils / String::Utils selective-import EXPORT, which
# resolves each requested name through `UNIT::` -- including a lexical `multi`
# family and an `in:out` rename.
my proto sub build(|) {*}
my multi sub build(Int:D $n) { "int $n" }
my multi sub build(Str:D $s) { "str $s" }
my sub plain($x) { "plain $x" }
my sub long-name($x) { "long $x" }

our sub probe() { build(42) }

my sub EXPORT(*@names) {
    Map.new: @names.map: {
        if UNIT::{"&$_"} -> &code {
            Pair.new("&$_", &code)
        }
        else {
            my ($in, $out) = .split(':', 2);
            if $out && UNIT::{"&$in"} -> &code {
                Pair.new("&$out", &code)
            }
        }
    }
}
