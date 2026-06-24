unit role URPort;
has Int:D $.port = 8080;
has Str:D $.host = '0.0.0.0';
method show() { "$.host:$.port" }
