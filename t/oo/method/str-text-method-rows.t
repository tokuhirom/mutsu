use Test;

# ADR-11276 slice 3: Str's zero-argument text methods are rows in the
# built-in method table. A plain Str answers from the row (also through the
# call-site lane, exercised by the loops), and so does every receiver whose
# MRO reaches Cool's copy of the method; allomorphs and Str subclasses keep
# their own answers.

plan 20;

my @methods = <uc lc fc tc tclc wordcase flip trim trim-leading trim-trailing chomp chop codes ord>;

subtest 'plain Str', {
    plan 14;
    my $s = "  hello wORLD ß\n";
    my %want =
        uc => "  HELLO WORLD SS\n",
        lc => "  hello world ß\n",
        fc => "  hello world ss\n",
        tc => "  hello wORLD ß\n",
        tclc => "  hello world ß\n",
        wordcase => "  Hello World Ss\n",
        flip => "\nß DLROw olleh  ",
        trim => "hello wORLD ß",
        trim-leading => "hello wORLD ß\n",
        trim-trailing => "  hello wORLD ß",
        chomp => "  hello wORLD ß",
        chop => "  hello wORLD ß",
        codes => 16,
        ord => 32;
    is-deeply $s."$_"(), %want{$_}, ".$_" for @methods;
}

subtest 'the call-site lane answers every iteration alike', {
    plan 4;
    my $s = "abc ";
    my @got;
    for ^3 { @got.push: $s.uc; @got.push: $s.flip; @got.push: $s.trim; @got.push: $s.ord }
    is-deeply @got[^4].List, ("ABC ", " cba", "abc", 97), 'first iteration';
    is-deeply @got[4..^8].List, @got[^4].List, 'second iteration';
    is-deeply @got[8..^12].List, @got[^4].List, 'third iteration';
    my @words = <a bb ccc>;
    is-deeply @words.map(*.uc).List, <A BB CCC>, 'a different receiver on each call';
}

subtest 'the empty string', {
    plan 14;
    is-deeply ""."$_"(), ($_ eq 'codes' ?? 0 !! $_ eq 'ord' ?? Nil !! ""), ".$_" for @methods;
}

is-deeply 42.5.flip, "5.24", 'Cool receiver: Rat.flip';
is-deeply 42.chop, "4", 'Cool receiver: Int.chop';
is-deeply 1e0.codes, 1, 'Cool receiver: Num.codes';
is-deeply [1, "b c "].flip, " c b 1", 'Cool receiver: Array.flip stringifies the list';
is-deeply %(a => 1).uc, "A\t1", 'Cool receiver: Hash.uc stringifies the pairs';
{
    my $i = 120;
    my @got = (^3).map({ $i.flip });
    is-deeply @got.List, ("021", "021", "021"), 'a Cool row through the call-site lane';
}
is-deeply IntStr.new(7, " ab c\n").trim, "ab c", 'an allomorph reads its Str part';

class MyStr is Str { }
is MyStr.new(value => "abc").flip, "cba", 'a Str subclass instance';

{
    my $s = "abc\n";
    is-deeply $s.chomp, "abc", 'chomp drops one line ending';
    is-deeply "abc\r\n".chomp, "abc", 'chomp drops a CRLF as one';
    is-deeply "abc".chomp, "abc", 'chomp leaves a string without one';
}

ok Str.^can($_), "Str.^can('$_')" for <uc flip trim chomp>;

use MONKEY-TYPING;
augment class Str { method shout() { self.uc ~ "!" } }
is "ab".shout, "AB!", 'a method augmented into Str calls a row on self';

is-deeply "abc".uc.lc.tc, "Abc", 'chained calls';
