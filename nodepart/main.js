var addon = require('./build/Release/nodeipc')

var xx = new addon.IpcConn(12313)

var http = require('http');
http.createServer(function (req, res) {
  res.writeHead(200, {'Content-Type': 'text/plain'});
  res.end('Hello World\n');
}).listen(1337, '192.168.2.102');
console.log('Server running at http://127.0.0.1:1337/');

console.log(xx.send)
//addon.ipcserver_listen("myipcserver", 1, function(client, event, data) {
//	console.log(client)
//}
//
//)

console.log("sdfsdf")
