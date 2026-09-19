`.^can('name')` no longer reports ClassHOW's own `name` meta-method for an
ordinary instance. This lets code that installs dynamic accessors on anonymous
classes distinguish an absent accessor from the metaobject API.
