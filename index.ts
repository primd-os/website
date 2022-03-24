import fs from 'fs';
import http from 'http';

const requestListener:http.RequestListener = function (req, res) {
    let data: Uint8Array[] = []
    let reqData: { [key: string]: any } = {}
    req
        .on("data", (d: Uint8Array) => {
            data.push(d)
        })
        .on("end", () => {
            reqData = data.length ? JSON.parse(Buffer.concat(data).toString()) : {}
            if (req.url?.includes("..")) {
                return;
            } else {
                let filename = "./website-interface" + req.url;

                fs.access(filename, function (err) {
                    if (err) {
                        res.writeHead(404, { "Content-Type": "text/plain" });
                        res.write("404 Not Found\n");
                        res.end();
                        return;
                    }

                    if (fs.statSync(filename).isDirectory()) filename += '/index.html';

                    fs.readFile(filename, "binary", function (err, file: string) {
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
        })
}

const server = http.createServer(requestListener);
server.listen(8080);