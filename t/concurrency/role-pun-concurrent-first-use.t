use Test;

# A role's pun class is composed the first time a method is called on the
# role, and found by name afterwards. Composing it takes several writes to the
# registry every thread shares, so a thread that asked for the same pun while
# another was still composing it found a class with half its methods missing:
# "No such method 'm'" (#18858, Monad's t/14_concurrent_types). Each role here
# is punned for the first time by every thread at once; any exception a
# thread meets is collected and fails the test, not lost with the thread.

plan 2;

role PR0[::T = Any] { method m(--> Str) { 'pr0' }; method k(--> Str) { T.^name } }
role PR1[::T = Any] { method m(--> Str) { 'pr1' }; method k(--> Str) { T.^name } }
role PR2[::T = Any] { method m(--> Str) { 'pr2' }; method k(--> Str) { T.^name } }
role PR3[::T = Any] { method m(--> Str) { 'pr3' }; method k(--> Str) { T.^name } }
role PR4[::T = Any] { method m(--> Str) { 'pr4' }; method k(--> Str) { T.^name } }
role PR5[::T = Any] { method m(--> Str) { 'pr5' }; method k(--> Str) { T.^name } }
role PR6[::T = Any] { method m(--> Str) { 'pr6' }; method k(--> Str) { T.^name } }
role PR7[::T = Any] { method m(--> Str) { 'pr7' }; method k(--> Str) { T.^name } }
role PR8[::T = Any] { method m(--> Str) { 'pr8' }; method k(--> Str) { T.^name } }
role PR9[::T = Any] { method m(--> Str) { 'pr9' }; method k(--> Str) { T.^name } }
role PR10[::T = Any] { method m(--> Str) { 'pr10' }; method k(--> Str) { T.^name } }
role PR11[::T = Any] { method m(--> Str) { 'pr11' }; method k(--> Str) { T.^name } }
role PR12[::T = Any] { method m(--> Str) { 'pr12' }; method k(--> Str) { T.^name } }
role PR13[::T = Any] { method m(--> Str) { 'pr13' }; method k(--> Str) { T.^name } }
role PR14[::T = Any] { method m(--> Str) { 'pr14' }; method k(--> Str) { T.^name } }
role PR15[::T = Any] { method m(--> Str) { 'pr15' }; method k(--> Str) { T.^name } }
role PR16[::T = Any] { method m(--> Str) { 'pr16' }; method k(--> Str) { T.^name } }
role PR17[::T = Any] { method m(--> Str) { 'pr17' }; method k(--> Str) { T.^name } }
role PR18[::T = Any] { method m(--> Str) { 'pr18' }; method k(--> Str) { T.^name } }
role PR19[::T = Any] { method m(--> Str) { 'pr19' }; method k(--> Str) { T.^name } }
role PR20[::T = Any] { method m(--> Str) { 'pr20' }; method k(--> Str) { T.^name } }
role PR21[::T = Any] { method m(--> Str) { 'pr21' }; method k(--> Str) { T.^name } }
role PR22[::T = Any] { method m(--> Str) { 'pr22' }; method k(--> Str) { T.^name } }
role PR23[::T = Any] { method m(--> Str) { 'pr23' }; method k(--> Str) { T.^name } }
role PR24[::T = Any] { method m(--> Str) { 'pr24' }; method k(--> Str) { T.^name } }
role PR25[::T = Any] { method m(--> Str) { 'pr25' }; method k(--> Str) { T.^name } }
role PR26[::T = Any] { method m(--> Str) { 'pr26' }; method k(--> Str) { T.^name } }
role PR27[::T = Any] { method m(--> Str) { 'pr27' }; method k(--> Str) { T.^name } }
role PR28[::T = Any] { method m(--> Str) { 'pr28' }; method k(--> Str) { T.^name } }
role PR29[::T = Any] { method m(--> Str) { 'pr29' }; method k(--> Str) { T.^name } }
role PL0 { method m(--> Str) { 'pl0' } }
role PL1 { method m(--> Str) { 'pl1' } }
role PL2 { method m(--> Str) { 'pl2' } }
role PL3 { method m(--> Str) { 'pl3' } }
role PL4 { method m(--> Str) { 'pl4' } }
role PL5 { method m(--> Str) { 'pl5' } }
role PL6 { method m(--> Str) { 'pl6' } }
role PL7 { method m(--> Str) { 'pl7' } }
role PL8 { method m(--> Str) { 'pl8' } }
role PL9 { method m(--> Str) { 'pl9' } }
role PL10 { method m(--> Str) { 'pl10' } }
role PL11 { method m(--> Str) { 'pl11' } }
role PL12 { method m(--> Str) { 'pl12' } }
role PL13 { method m(--> Str) { 'pl13' } }
role PL14 { method m(--> Str) { 'pl14' } }
role PL15 { method m(--> Str) { 'pl15' } }
role PL16 { method m(--> Str) { 'pl16' } }
role PL17 { method m(--> Str) { 'pl17' } }
role PL18 { method m(--> Str) { 'pl18' } }
role PL19 { method m(--> Str) { 'pl19' } }
role PL20 { method m(--> Str) { 'pl20' } }
role PL21 { method m(--> Str) { 'pl21' } }
role PL22 { method m(--> Str) { 'pl22' } }
role PL23 { method m(--> Str) { 'pl23' } }
role PL24 { method m(--> Str) { 'pl24' } }
role PL25 { method m(--> Str) { 'pl25' } }
role PL26 { method m(--> Str) { 'pl26' } }
role PL27 { method m(--> Str) { 'pl27' } }
role PL28 { method m(--> Str) { 'pl28' } }
role PL29 { method m(--> Str) { 'pl29' } }

constant THREADS = 12;
my @roles = PR0, PR1, PR2, PR3, PR4, PR5, PR6, PR7, PR8, PR9, PR10, PR11, PR12, PR13, PR14, PR15, PR16, PR17, PR18, PR19, PR20, PR21, PR22, PR23, PR24, PR25, PR26, PR27, PR28, PR29, PL0, PL1, PL2, PL3, PL4, PL5, PL6, PL7, PL8, PL9, PL10, PL11, PL12, PL13, PL14, PL15, PL16, PL17, PL18, PL19, PL20, PL21, PL22, PL23, PL24, PL25, PL26, PL27, PL28, PL29;
my $errors-lock = Lock.new;
my @errors;
my @results;
my $go = Promise.new;

my @threads = (^THREADS).map: -> $n {
    Thread.start({
        await $go;
        my @got;
        for @roles.rotate($n * 3) -> \R {
            @got.push: R.m;
            CATCH { default { $errors-lock.protect: { @errors.push: "thread $n: {.message}" } } }
        }
        $errors-lock.protect: { @results.push: @got.sort.join(' ') };
    })
};
$go.keep;
.finish for @threads;

is @errors.elems, 0, 'no thread met a missing method on a pun composed concurrently'
    or diag @errors.head(5).join("\n");
is-deeply @results.unique.List, ('pl0 pl1 pl10 pl11 pl12 pl13 pl14 pl15 pl16 pl17 pl18 pl19 pl2 pl20 pl21 pl22 pl23 pl24 pl25 pl26 pl27 pl28 pl29 pl3 pl4 pl5 pl6 pl7 pl8 pl9 pr0 pr1 pr10 pr11 pr12 pr13 pr14 pr15 pr16 pr17 pr18 pr19 pr2 pr20 pr21 pr22 pr23 pr24 pr25 pr26 pr27 pr28 pr29 pr3 pr4 pr5 pr6 pr7 pr8 pr9',),
    'every thread got every role\'s answer';
