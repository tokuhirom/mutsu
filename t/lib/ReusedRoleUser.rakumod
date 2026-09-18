use v6.c;
use ReusedRoleHandler;
use ReusedRoleCommon;

unit class ReusedRoleUser;

grammar ReusedRoleGrammar does Reuse-Role {
    rule TOP { . }
}

method greet-via-grammar { ReusedRoleGrammar.greet }
