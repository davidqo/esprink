# esprink
File caster server.
Client doesn't implemented yet.

To build server you need:
```
git clone
make
```
Currently the only one way to start streaming:
```
make run
```
And enter this code in opened erlang console:

```erlang
esprink:add_session(<<"session1">>, #{filename => "testfile", bytes_per_sec => 1024, max_chunk_size => 512, local_port => 9999, remote_port => 1111, remote_address => "192.168.23.46"}).
```

Optionally you can specify local_address and local_port options.
It doesn't support multicast yet, but designed for this

What is supported:

1). Run several streams simultaneously;
2). Run different sterams with different bytes_per_sec and mach_chunk_size options;
3). Using the same file in the different streams;
4). Fault-tolerance code

What is not yet implemented, but designed for:

1). Client. You can analize streams only through wireshark;
2). Console command with command line interface;
3). HTTP Server, showing currently running streams;
4). Multicast streaming;
5). Retransmissions on demand;
6). Release building
7). Logging

NOTES:

I purposely use bytes_per_second instead of bps because of the ambiguity of the latter (bytes_per_second? bits_per_second? bits_per_sample?).
