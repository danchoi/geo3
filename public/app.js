var websocket;
function startWebSocket() {
  var webSocketURL = 'ws://localhost:9160/ws'; 
  websocket = new WebSocket(webSocketURL); 
  websocket.onopen = function(event){
    console.log("Connected to server");
  };
  websocket.onmessage = function(event){
    console.log(event);
  };
  
  websocket.onclose = function(event){
    console.log("connection closed");
  };
};



$(document).ready(function() {
  startWebSocket();
});

