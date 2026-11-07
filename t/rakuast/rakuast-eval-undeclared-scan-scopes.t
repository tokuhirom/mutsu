use Test;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;

# The EVAL undeclared-variable scan knows two scopes it used to miss: a
# statement-form phaser declares into its block (`NEXT (state $best) max= $_`
# is visible to a later `LAST`), and a method declared in a nested block of a
# class body names its explicit invocant (`method m ($class: |c)`). Checked
# against rakudo, both as a string and as `EVAL($ast)`.

plan 5;

my $phaser = Q[my $out = '';
for (3, 2, 42) -> $score {
    NEXT (state $best) max= $score;
    LAST $out ~= "best=$best";
}
$out];

is EVAL($phaser), 'best=42', 'a statement-form NEXT declares a state for LAST (string)';
is EVAL($phaser.AST), 'best=42', 'a statement-form NEXT declares a state for LAST (AST)';

my $invocant = Q[class Reg {
    {
        my $instance;
        method instance ($class: |c) { return $instance //= $class.new(|c) }
    }
}
Reg.instance.WHAT.^name];

is EVAL($invocant), 'Reg', 'a nested-block method names its explicit invocant (string)';
is EVAL($invocant.subst('Reg', 'RegB', :g).AST), 'RegB', 'a nested-block method names its explicit invocant (AST)';

is (try EVAL(Q[{ NEXT $oops = 2; }; 1])), Nil, 'a statement-form phaser does not declare an unrelated name';
