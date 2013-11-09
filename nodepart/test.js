//var bert = require('./bert')
var main = require('./main')
var bert = main.bert
function string_to_buffer (string) {
      return new Buffer(string, 'binary');
}

function test(client, args) {
	console.log('sdfsdfffffffffffffffffffffffffffffff')
	main.call_erlang(client, client.pid, bert.atom("test"), bert.atom("test"), [],
		function (cb_result) {
			console.log(cb_result)
			var result = bert.encode(args[0])
			client.resp(string_to_buffer(result))
		})
}

module.exports.test_fun = test
