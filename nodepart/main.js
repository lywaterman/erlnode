var addon = require('./build/Release/nodeipc')

addon.ipcserver_listen("myipcserver", 1, function(event, data) {
	console.log(data)
}

)

console.log("sdfsdf")
