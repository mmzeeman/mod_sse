/* z.sse_stream.js */

z_stream = undefined;

function z_sse_start() {
    var url = "/stream?z_pageid=" + urlencode(z_pageid);
    z_stream = new EventSource(url);
    z_stream.addEventListener("script", handle_script);
}

function handle_script(e) {
    z_comet_data(e.data);
}
