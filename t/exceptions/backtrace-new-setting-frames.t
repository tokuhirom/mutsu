use Test;

# From Proc::Easier: it finds its caller by skipping to the first setting
# frame that is not Backtrace's own. Rakudo's Backtrace.list starts with the
# `Backtrace.new` setting frame, and a BUILD runs under POPULATE <- Mu.new.

plan 6;

my @bt = Backtrace.new.list;
ok @bt[0].file.starts-with('SETTING'), 'Backtrace.new frame heads the list';
is @bt[0].subname, 'new', '... named new';

class Foo {
    has $.where;
    submethod BUILD() { $!where = Backtrace.new.list.map(*.subname).join(',') }
}
my $names = Foo.new.where;
like $names, /'BUILD,POPULATE,new'/, 'BUILD sits over POPULATE and Mu.new';

class Bar {
    has $.line;
    submethod BUILD() {
        my @b = Backtrace.new.list;
        my @s = @b.grep({ .file.starts-with('SETTING') && !.file.contains('Backtrace') }, :k);
        $!line = @b[@s[0] + 1].line;
    }
}
my $b = Bar.new;
is $b.line, $?LINE - 1, 'caller located one frame past the first non-Backtrace setting frame';
ok Bar.new.line, 'second construction too';

sub plain { Backtrace.new.list.map(*.subname).join(',') }
is plain(), 'new,plain,<unit>', 'plain sub backtrace';
