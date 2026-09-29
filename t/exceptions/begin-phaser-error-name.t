use Test;

plan 4;

# An exception thrown in a BEGIN/CHECK phaser is wrapped in X::Comp::BeginTime,
# whose message names the phaser that was running (#9917).
for <BEGIN CHECK> -> $ph {
    try EVAL "$ph \{ die 'x' \}";
    isa-ok $!, X::Comp::BeginTime, "$ph: error is X::Comp::BeginTime";
    like $!.message, /"while evaluating a $ph"/, "$ph: message names the phaser";
}

done-testing;
