var sendurl = 'http://127.0.0.1:8000/message';
var geturl = 'http://127.0.0.1:8000/message';

function loadjs(url, callback) {
    var _doc = $$('head')[0];
    var js = new Element('script');
    js.type = 'text/javascript';
    js.src = url;
    if(callback == null) {
        callback = function(){};
    }
    var _callback = function(){
            callback();
            js.destroy();
        };

    if (!/*@cc_on!@*/0) { //if not IE
        //Firefox2、Firefox3、Safari3.1+、Opera9.6+ support js.onload
        js.onload = function () {
            _callback();
        }
    } else {
        //IE6、IE7 support js.onreadystatechange
        js.onreadystatechange = function () {
            if (js.readyState == 'loaded' || js.readyState == 'complete') {
                _callback();
            }
        }
    }
    _doc.adopt(js);
    return false;
}

function proxy(ifp) {
    if(ifp == null) {
        ifp = $('proxy');
    }
    var _p;
    if (ifp.contentDocument) {
        _p = ifp.contentDocument;
    } else if (ifp.contentWindow) {
    // For IE5.5 and IE6
        if(ifp.contentWindow.document) {
            _p = ifp.contentWindow.document;
        } else {
            _p = ifp.contentWindow;
        }
    } else if (ifp.document) {
    // For IE5    
        _p = ifp.document;
    }
    return _p;
}

var iframe_callbacked;

function loadiframe(callback) {
    var _doc = $('iframe');
    if($('proxy')) {
        $('proxy').destroy();
    }
    var ifr = new Element('iframe', {
            'id': "proxy",
            'src': "proxy.htm",
            'frameborder': "0",
            'hspace': "0",
            'vspace': "0",
            'scrolling': "no",
            'style': "display:none"
        });
        
    iframe_callbacked = false;
    
    if(callback == null) {
        callback = function(){};
    }
    var _callback = function(){
            if(iframe_callbacked == false) {
                callback();
                iframe_callbacked = true;
            }
        };

    if (!/*@cc_on!@*/0) { //if not IE
        //Firefox2、Firefox3、Safari3.1+、Opera9.6+ support js.onload
        ifr.onload = function () {
            _callback();
        }
    } else {
        //IE6、IE7 support js.onreadystatechange
        ifr.onreadystatechange = function () {
            if (ifr.readyState == 'loaded' || ifr.readyState == 'complete') {
                _callback();
            }
        }
    }
    
    _doc.adopt(ifr);
    return false;
}

function log(str) {
    $('log').innerHTML += str + "<br/>";
}


var msgform = function(){
        var _mf = proxy().getElementById('msgform');
        _mf.action = sendurl;
        return _mf;
    };
var msg = function() {return proxy().getElementById('message');};

function form_send() {
    try {
        loadiframe(function() {
                msg().value = $('message').value;
                msgform().submit();
            });
    } catch(e) {
        alert(typeof(e)+': '+e);
    }
}

function refresh(msg) {
    var item = new Element('div');
    item.innerHTML = msg;
    $('records').adopt(item); 
}


function getmsg() {
    var _url = geturl+'?'+new Date().getTime();
    try{
        loadjs(_url, function() {
                var r = response();
                if(r.length) {
                    refresh(r);
                }
                msgmain();
            });
    } catch(e) {
        alert(typeof(e)+': '+e);
    }
}

function msgmain() {
    setTimeout(function(){getmsg();}, 0);
}

function onload() {
    var href = new String(location.href);
    var p = href.indexOf("?");
    var tag = "/";
    if(p != -1) {
        tag = href.substring(p + 1);
        if(tag != "") {
            geturl += "/"+tag;
            sendurl += "/"+tag;
        }
    }
    msgmain();
}
