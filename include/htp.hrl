-record(client, {
    transport,
    htp_module,
    host,
    port,
    method,
    buffer = <<>>,
    version,
    status = closed,
    response_body,
    socket,
    options = [],
    connect_timeout = 5000,
    response_timeout = 10000,
    http_version = {1, 0},
    connection
}).
