var websocket, name;
function startWebSocket() {
  var webSocketURL = 'ws://localhost:9160/ws'; 
  websocket = new WebSocket(webSocketURL); 
  websocket.onopen = function(event){
    console.log("Connected to server " );
    if (name) 
      websocket.send("rename to " + name);

    websocket.onmessage = function(event){
      console.log("onmessage:" + event.data);
      if (/^\w+$/.test(event.data)) {
        name = event.data;
      } 
    };
    websocket.onclose = function(event){
      console.log("connection closed. reconnecting.");
      // try to reopen
      setTimeout(startWebSocket, 1000);
    };

  };
};

/* Important to prevent accmulation of ghost websockets */
$(window).unload(function() {
  if (websocket) websocket.close();
  return;
});


$(document).ready(function() {
  startWebSocket();
});

var map = L.map('map', { dragging: true,
                         zoomControl: true,
                         zoomAnimation: false,
                         scrollWheelZoom: false,
                         doubleClickZoom: false,
                         touchZoom: false
                       }).setView([42.375, -71.106], 14);


L.tileLayer('http://{s}.tile.cloudmade.com/' + API_KEY + '/997/256/{z}/{x}/{y}.png', {
    attribution: 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery © <a href="http://cloudmade.com">CloudMade</a>',
    maxZoom: 18
}).addTo(map);

function printBounds(b) {
  var sw = b.getSouthWest(),
      se = b.getSouthEast(),
      ne = b.getNorthEast(),
      nw = b.getNorthWest();
  console.log([sw,se,ne,nw].toString());
}
map.on('moveend', function(e) {
  var c = map.getCenter(),
      lat = c.lat,
      lng = c.lng,
      z = map.getZoom(),
      loc = [lat,lng,z].join(' ');
  websocket.send("loc " + loc);
});


var svg = d3.select(map.getPanes().overlayPane).
            append("svg").
            attr('class', 'myMapOverlay'),
    g = svg.append("g").attr("class", "leaflet-zoom");


