var fs = require('fs');
var http = require('http');
var url = require('url');

var host = "192.168.2.166";
var port = 1337;
var attitudes = {};
var server = http.createServer( function(req, res) {

    //console.dir(req.param);
/*
    if (req.method == 'POST') {
        console.log("POST");
        var body = '';
        req.on('data', function (data) {
            body += data;
            console.log("Partial body: " + body);
        });
        req.on('end', function () {
            console.log("Body: " + body);
        });
        res.writeHead(200, {'Content-Type': 'text/html'});
        res.end('post received');
    }
    else
*/
    {
        // console.log("GET");
	var url_parts = url.parse(req.url, true);
	var query = url_parts.query;
       	if (query.id && query.q0) {
	  var id = query.id;
	  attitudes[id] = [query.q0, query.q1, query.q2];
        } 
        str = JSON.stringify(attitudes);
	console.log(str);
	var html = '<html><body>'+ str + '<br>' + '</body>';
        //var html = fs.readFileSync('index.html');
        res.writeHead(200, {'Content-Type': 'text/html'});
        res.end(html);
    }

});

server.listen(port, host);
console.log('Server running at http://' + host + ':' + port + '/');
