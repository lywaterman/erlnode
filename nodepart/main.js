var addon = require('./build/Release/nodeipc')

var bert = require('./bert')

var yy = bert.encode({"abc":1})

console.log(typeof yy)

console.log(bert.decode(yy))

var xx = new addon.IpcConn(12313)

var http = require('http');
http.createServer(function (req, res) {
  res.writeHead(200, {'Content-Type': 'text/plain'});
  res.end('Hello World\n');
}).listen(1337, '192.168.2.102');
console.log('Server running at http://127.0.0.1:1337/');

console.log(xx.send)
addon.ipcserver_listen("myipcserver", 1, function(client, event, data) {
	client.send("hello erlang")
}

)

console.log("not block")
