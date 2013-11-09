var addon = require('./build/Release/nodeipc')
var uuid = require('node-uuid')

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

//pid callback
//from erl to node
nodeerl_callbacks = {


}

function get_op(data) {
	return bert.decode(data.toString('binary'))
}

function string_to_buffer (string) {
      return new Buffer(string, 'binary');
}

function call_erlang(client, pid, M, F, A, callback) {
	console.log('2333333333333333333')
	var callback_uuid = uuid.v1()
	nodeerl_callbacks[callback_uuid] = callback
	console.log(pid)
	var msg = bert.encode(bert.tuple(bert.atom('callback'), callback_uuid, M, F, A))
	client.call(pid, string_to_buffer(msg))
}

function excute_cb(client, op) {
	console.log('2222222222222222222222222222222222222222222')
	//先看是否为call
	if (op.type != 'tuple' || op.value[0] != 'callback') {
		return
	}
	
	console.log("callback")
	var cb_uuid = op.value[1]
	var cb_result = op.value[2]
	
	if (nodeerl_callbacks[cb_uuid] != undefined && 
	    nodeerl_callbacks[cb_uuid] != null) {
		nodeerl_callbacks[cb_uuid](cb_result)
		delete nodeerl_callbacks[cb_uuid]
	}

}

function excute_call(client, op) {
	if (op.type != 'tuple' || op.value[0] != 'call') {
		return
	}

	console.log("is call")
	var module = require(op.value[1])
	var fun_name = op.value[2]
	var args = op.value[3]	
	module[fun_name](client, args)
}

addon.ipcserver_listen("myipcserver", 1, function(client, event, data, pid) {
	console.log('---------------------------------------------------')
	console.log("pid:" + pid)
	client.pid = pid
	if (event == "data") {		
		var op = get_op(data)	
		console.log(op)
		var res = excute_call(client, op)
	} else if (event == "callback") {
		var op = get_op(data)
		console.log(op)	

		console.log('---------------------------------------------------')
		excute_cb(client, op)	
	}
}

)
console.log("not block")


module.exports.bert = bert
module.exports.call_erlang = call_erlang
