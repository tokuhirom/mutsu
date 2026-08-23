use Test;

plan 1;

1.Str::split(/a/);
CATCH {
    default {
        is .Str, 'Cannot dispatch to method split on Str because it is not inherited or done by Int',
            'InvalidQualifier includes the method name';
    }
}
