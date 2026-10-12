use Test;

# ADR-11276 §9.79: a receiver with no dispatch shape reaches the method
# table's rows without the cascade arms that used to stringify it.
#
# - An instance of a user class deriving from a built-in scalar type is
#   answered by the rows of its payload's shape (the native-base resolution).
# - A shapeless Cool (an enum value, a Seq, an instance of a class `is Cool`)
#   reaches the rows Cool declares.
# - The closed Range, Seq and IO::Path shapes reach Cool's text rows.
# - A receiver that is no Cool (a Pair, a Set, a Sub) does not.

plan 9;

class MyStr is Str { }
class MyInt is Int { }

subtest 'a Str subclass instance answers the Str rows on its payload', {
    # Rakudo boxes some of these answers into MyStr; mutsu answers a Str,
    # so the text, not the type, is compared.
    plan 10;
    my $s = MyStr.new(value => "  ab c\n");
    is $s.flip, "\nc ba  ", 'flip';
    is $s.trim, "ab c", 'trim';
    is $s.chomp, "  ab c", 'chomp';
    is $s.naive-word-wrapper, "ab c", 'naive-word-wrapper';
    is $s.uc, "  AB C\n", 'uc';
    is $s.words.join('|'), "ab|c", 'words';
    is $s.ord, 32, 'ord';
    is $s.codes, 7, 'codes';
    throws-like { MyStr.new(value => "x").Date }, X::Temporal::InvalidFormat, 'Date parses the payload';
    is $s.WHAT.^name, 'MyStr', 'the instance itself still answers WHAT';
}

subtest 'an Int subclass instance', {
    plan 4;
    my $i = MyInt.new(120);
    is-deeply $i.flip, "021", 'flip';
    is-deeply $i.chop, "12", 'chop';
    is $i.Int.^name, 'MyInt', 'Int answers the instance itself';
    ok $i.is-prime == False, 'is-prime on the payload';
}

subtest 'an enum value reaches the Cool rows', {
    plan 4;
    enum Color <red green>;
    is-deeply red.flip, "der", 'flip';
    is-deeply green.uc, "GREEN", 'uc';
    is-deeply red.comb.List, ("r", "e", "d"), 'comb';
    is-deeply green.trim-leading, "green", 'trim-leading';
}

subtest 'an instance of a class is Cool', {
    plan 2;
    class CC is Cool { }
    my $c = CC.new;
    like $c.flip, /^ '>' \d+ '<CC' $/, 'flip of its string form';
    like $c.uc, /^ 'CC<' \d+ '>' $/, 'uc of its string form';
}

subtest 'Range and Seq reach the Cool text rows', {
    plan 6;
    is-deeply (1..3).flip, "3 2 1", 'Range.flip';
    is-deeply (1..3).uc, "1 2 3", 'Range.uc';
    is-deeply (1, 2).Seq.chop, "1 ", 'Seq.chop';
    is-deeply <a b>.map(*.uc).lc, "a b", 'a mapped Seq';
    is-deeply (1..3).ords.List, (49, 32, 50, 32, 51), 'Range.ords';
    is-deeply (1..3).fmt, "1 2 3", 'Range.fmt is its own row';
}

subtest 'IO::Path reaches the Cool text rows, its own lines/words first', {
    plan 5;
    my $p = "x y\n".IO;
    is-deeply $p.trim, "x y", 'trim';
    is-deeply $p.flip, "\ny x", 'flip';
    is-deeply $p.chomp, "x y", 'chomp';
    is-deeply $p.codes, 4, 'codes';
    throws-like { "no-such-file-for-shapeless-cool".IO.lines.eager }, Exception, 'lines reads the file';
}

subtest 'a mixin reaches the rows of its inner value', {
    plan 2;
    my $s = "ab " but role { method r { 1 } };
    is $s.flip, " ba", 'flip';
    is $s.uc, "AB ", 'uc';
}

subtest 'a receiver that is no Cool has no Cool text method', {
    plan 4;
    throws-like { (a => 1).flip }, X::Method::NotFound, 'Pair';
    throws-like { set(<a>).trim }, X::Method::NotFound, 'Set';
    throws-like { sub { }.uc }, X::Method::NotFound, 'Sub';
    throws-like { \(1, 2).chop }, X::Method::NotFound, 'Capture';
}

{
    class Plain { }
    throws-like { Plain.new.flip }, X::Method::NotFound, 'a plain user class keeps X::Method::NotFound';
}

