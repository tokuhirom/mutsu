unit module NextcalleeImports::Scoped;
role Scoped is export {
    method prepare(|c) {
        my &next = nextcallee;
        next(self, |c)
    }
}
