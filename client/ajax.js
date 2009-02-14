var createAjax = function () {
    try {
        new ActiveXObject('MSXML3.XMLHTTP');
        return function() {
            return new ActiveXObject('MSXML3.XMLHTTP');
        }
    } catch(e) {}
    try {
        new ActiveXObject('MSXML2.XMLHTTP.3.0');
        return function() {
            return new ActiveXObject('MSXML2.XMLHTTP.3.0');
        }
    } catch(e) {}
    try {
        new ActiveXObject('Msxml2.XMLHTTP');
        return function() {
            return new ActiveXObject('Msxml2.XMLHTTP');
        }
    } catch(e) {}
    try {
        new ActiveXObject('Microsoft.XMLHTTP');
        return function() {
            return new ActiveXObject('Microsoft.XMLHTTP');
        }
    } catch(e) {}
    try {
        new XMLHttpRequest();
        return function() {
            return new XMLHttpRequest();
        }
    } catch(e) {}
    throw new Error('Could not find XMLHttpRequest or an alternative.');
}();

function Ajax(init) {
    if(init == null) {
        init = {url: ''};
    }
    
    this._base = createAjax();
    this.url = init.url;
    if('async' in init) {
        this.async = init.async;
    } else {
        this.async = true;
    }
    if('onComplete' in init) {
        this.onComplete = init.onComplete;
    } else {
        this.onComplete = function(_){};
    }
}

Ajax.prototype._packetdata = function(data) {
    var _d = '';
    if(data == null) {
        return _d;
    }
    if(typeof(data) == 'string') {
        return data;
    }
    if(data instanceof Element) {
        var id = data.get('id');
        var keys = $$('#'+id+' input');
        for(var i = 0; i < keys.length; i++) {
            k = keys[i].name;
            v = keys[i].value;
            if(k.length) {
                if(_d != '') {
                    _d += '&';
                }                
                _d += k+'='+encodeURIComponent(v);
            }
        }
        return _d;
    }
    if(typeof(data) == object) {
        for(k in data) {
            if(_d != '') {
                _d += '&';
            }
            _d += k+'='+encodeURIComponent(data[k]);
        }
        return _d;
    }
    return _d;
}
Ajax.prototype._send = function(method, data){
    if(method == 'POST') {
        log(method);
        log(data);
    }
    _d = this._packetdata(data);
    if(method == 'POST') {
        log('_d: ' + _d);
    }    
    this._base.open(method, this.url, this.async);
    if(method == 'POST') {
        this._base.setRequestHeader('Content-Type',	
            'application/x-www-form-urlencoded; charset=utf-8');
    }
    self = this;
    this._base.onreadystatechange = function() {
        if(self._base.readyState == 4) {
            self.onComplete(self._base.responseText, self._base.responseXML);
        }
    };
    this._base.send(_d);
}

Ajax.prototype.get = function(data) {
    this._send('GET', data);
    return this;
}
Ajax.prototype.post = function(data) {
    this._send('POST', data);
    return this;
}
