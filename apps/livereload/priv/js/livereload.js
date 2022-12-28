


class LiveReload{

    

    static start(args) {

        args = args || {}
        args.port = args.port || 10101
        args.path = args.path
        args.exts = args.exts || "*"

        args.tryLimit = 20
        args.startTimeout = args.startTimeout || 1000
        args.retryTimeout = args.retryTimeout || 300
        args.testUrl = args.testUrl || document.href


        if(!args.path || args.path == ""){
            console.error("Live Reload: invalid path. can't start monitor")
            return
        }

        console.log("Live Reload:", JSON.stringify(args))


        var url = "ws://localhost:" +  args.port + "/ws?path=" +  args.path + "&exts=" +  args.exts

        let liveReload = new WebSocket(url)

        function checkServerIsUp(successCb, errorCb){
            var xmlHttp = new XMLHttpRequest(args.testUrl);
            xmlHttp.onreadystatechange = function() {
                if (xmlHttp.readyState == 4 && xmlHttp.status == 200)
                    successCb()
                else
                    errorCb()
                    //callback(xmlHttp.responseText);
            }
            xmlHttp.open("GET", location.href, true); // true for asynchronous
            xmlHttp.send(null);
        }

        function pageReload(max){

            if(max == 0){
                console.error("Live Reload: reload limit found")
                return
            }

            checkServerIsUp(() => {
                location.reload()
            }, () => {
                setTimeout(() => {
                    pageReload(max-1)
                }, args.retryTimeout)
            })
        }

        liveReload.onopen = function (event) {
            console.log("Live Reload: Enabled.")
        }
        liveReload.onclose = function (event) {
            console.log("Live Reload: Closed.")
        }
        liveReload.onmessage = function (event) {
            let data = JSON.parse(event.data)
            let eventKey = data.event
            if (eventKey === "ping") {
                return
            }
            if (eventKey === "reload") {
                liveReload.close()
                setTimeout(() => {
                    pageReload(args.tryLimit)
                }, args.startTimeout)
            } else if (eventKey === "log") {
                let level = data.level
                let message = data.message
                if (level === "info") console.info(message)
                else if (level === "log") console.log(message)
                else if (level === "warn") console.warn(message)
                else if (level === "error") console.error(message)
                else console.log("Live Reload:", message)
            } else {
                console.log("Live Reload: Unknown message: " + data)
            }
        }
        
    };

}
