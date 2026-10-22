use EvalRoleImportHelper;
role EvalRoleImportRole {
    sub host { eval-role-helper() }
    my constant H = host();
    method h { H }
}
