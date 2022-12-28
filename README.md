Live site mon
=====

An OTP application

Build
-----

    $ rebar3 compile

Start
-----
    
    $ rebar3 shell


Monitore files
==============

Add on page

    <script type="text/javascript" src="http://localhost:10101/js/livereload.js"></script>
    <script type="text/javascript">
        LiveReload.start({
            path: "path/to/monitore"
        })          
    </script>
    

LiveReload args

    port: optional, default 10101
    path: required
    exts: optional, default *
    startTimeout: optional, default 1000 - First try after reaload event
    retryTimeout: optional, default 300 - Next try after reload event
    tryLimit: optional, default 20 - Reload try limit before stop
    testUrl: optional, default document.href
