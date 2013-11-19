//////// serial port:

function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = '0.0.0.0';
    var port = window.location.port;
    if(port == '') port = '8000';

    var uri = 'ws://' + host + ':' + port + path;

    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}

function addSerialMsg(txt){
   // Weiß nicht was das Zgw ausspuckt, und der Handler für hGetLine hat nicht funktioniert,
   // deswegen hab ich mir keine Gedanken über die Formatierung des SerialPort outputs gemacht

   $('#serial-out').append(txt);
   $('#serial-out').animate({scrollTop: $('#serial-out')[0].scrollHeight});
}

function initSerialSocket(){
    var ws = createWebSocket('/initSerialPort');

    ws.onmessage = function(event) {
	addSerialMsg(event.data)
    };
}

//////// diag msg:

function addMsg(txt){
   var p = $(document.createElement('p')).text(txt);
   $('#messages').append(p);
}

function sendDiagMsg(event){
    event.preventDefault(); 

    var $form = $( this )
    url = $form.attr( 'action' );
    tgt = $form.find( 'input[name="tgt"]' ).val();
    src = $form.find( 'input[name="src"]' ).val();
    ip  = $form.find( 'input[name="ip"]'  ).val();
    msg = $form.find( 'input[name="msg"]' ).val();

    $.post( url, { src: src, tgt:tgt, ip:ip, msg:msg },
      function( data ) {
	  $('#messages').append('<font size="-2">Msg:</font><br>');
	  addMsg(msg);
	  $('#messages').append('<font size="-2">Response:</font><br>');
	  addMsg(data);
	  $('#messages').append('<hr/>');
          $('#messages').animate({scrollTop: $('#messages')[0].scrollHeight});
      }
    );
}

///////// config:

function setConfigVal(name, val) {
    $("#message-form").find('input[name="' + name+ '"]').val(val)
}

function setDefaultConfig(data){
    var jsn = jQuery.parseJSON(data);
    setConfigVal("ip",  jsn.ip);
    setConfigVal("tgt", jsn.tgt);
    setConfigVal("src", jsn.src);
    $("#serial-port-path").text('"' + jsn.serialPath +'"');
};

function getDefaultConfig() {
    $.get('defaultConfig', setDefaultConfig);
};


///////// init

$(document).ready(function () {
    getDefaultConfig();
    $("#message-form").submit(sendDiagMsg);
    initSerialSocket();
});



