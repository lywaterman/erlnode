erlnode
=======

erlang nodejs 

This is a library to make erlang and nodejs to communicate each other;

It is made of two parts, erlang part and node part;

It use ipc_shm to communicate, it is very fast;

It use bert proto

I will build like call, load, eval on top on bert, and make erlang call node, or node call erlang
like a embed call


|||||||||         call eval etc
|||||||||         bert proto     ||||||||||  
erlang||| <-------ipc_shm------->|||node|||
|||||||||                        ||||||||||
