use Test;

plan 2;

# ComfyUI::API's Workflow.render() relies on an exception from a .subst()
# replacement block to reject a missing template variable.
dies-ok {
    'hello {{missing}}'.subst(/ '{{' (<-[}]>+) '}}' /, -> $/ {
        die "missing variable '{~$0}'";
    });
}, 'regex replacement block propagates its exception';

dies-ok {
    'hello'.subst('hello', {
        die 'replacement failed';
    });
}, 'string replacement block propagates its exception';
