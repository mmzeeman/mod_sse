mod_sse
=======

Zotonic module implementing Server Sent Events

Introduction
------------

Server-sent events (SSE) is a push technologiy where a browser gets automatic updates from a server via HTTP connection. The Server-Sent Events EventSource API is standardized as part of HTML5 by the W3C.

This module consists of a module, ```z_sse```, implementing the SSE message protocol. This module can be used to implement custom SSE controllers. An example controller, ```controller_stream``` is also included. This controller can be used as drop-in replacement for the standard ```{% stream %}``` scomp. Instead of using a standard Comet and Websocket connection an SSE connection will be used.

SSE is supported by all newer browsers except Internet Explorer. A polyfill js library is included in order to implement SSE streams for browsers which do not support it.

### The Good

  - Push technology based on standard HTTP.
  - Easy to use on the client and server side. Clients handles automatic re-connects, which are server directed.
  - All modern browsers except IE have native implementation. IE is supported via polyfill js library.
  - Easily usable by external tools because it is standard HTTP.

### The Bad

Eventhough SSE is implemented as standard HTTP I have found that you can run into problems with security software. I have found that at least F-Secure's "Advanced Network Protection" feature blocks any SSE requests from competing. The response headers are never returned to the browser. It could be that other security products also block SSE requests.



