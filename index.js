"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var fs_1 = __importDefault(require("fs"));
var http_1 = __importDefault(require("http"));
var requestListener = function (req, res) {
    var data = [];
    var reqData = {};
    req
        .on("data", function (d) {
        data.push(d);
    })
        .on("end", function () {
        var _a;
        reqData = data.length ? JSON.parse(Buffer.concat(data).toString()) : {};
        if ((_a = req.url) === null || _a === void 0 ? void 0 : _a.includes("..")) {
            return;
        }
        else {
            var filename_1 = "./website-interface" + req.url;
            fs_1.default.access(filename_1, function (err) {
                if (err) {
                    res.writeHead(404, { "Content-Type": "text/plain" });
                    res.write("404 Not Found\n");
                    res.end();
                    return;
                }
                if (fs_1.default.statSync(filename_1).isDirectory())
                    filename_1 += '/index.html';
                fs_1.default.readFile(filename_1, "binary", function (err, file) {
                    if (err) {
                        res.writeHead(500, { "Content-Type": "text/plain" });
                        res.write(err + "\n");
                        res.end();
                        return;
                    }
                    res.writeHead(200);
                    res.write(file, "binary");
                    res.end();
                });
            });
        }
    });
};
var server = http_1.default.createServer(requestListener);
server.listen(8080);
//# sourceMappingURL=index.js.map