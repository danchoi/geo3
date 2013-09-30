var sessionPrefix;
var COOKIE_DAYS = 3;


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


function ChatController($scope, $http, $log) {

  // populate from cookie values, if they exist 
  sessionPrefix = readCookie("sessionprefix");
  $scope.nickname = readCookie("nickname");

  $scope.submitNick = function() {
    if (!sessionPrefix) 
      $scope.connect($scope, $scope.nickname);
    else 
      $http.post('/events', sessionPrefix + ' rename to '+$scope.nickname);
    createCookie("nickname", $scope.nickname, COOKIE_DAYS);
  }

  $scope.connect = function() {
    var data = "- connect "+$scope.nickname;
    console.log("connecting..");
    $http.post("/events", data).success(function(data) {
      $log.log(data); 
      sessionPrefix = data.uuid + ' ' + data.session;
      createCookie("sessionprefix", sessionPrefix, COOKIE_DAYS);
    }); 
  }

  $scope.chat = function() {

  }

  // TODO make interaction
  $scope.logout = function() {
    clearCookies();
    $scope.nickname = null;
    sessionPrefix = null;
  }

  map.on('moveend', function(e) {
    if (!sessionPrefix) return;
    var c = map.getCenter(),
        lat = c.lat,
        lng = c.lng,
        zoom = map.getZoom(),
        loc = [lat,lng,zoom].join(' '),
        msg = sessionPrefix+' move to '+loc;
    $log.log(msg);
    $http.post('/events', msg);
  });


}

function clearCookies() {
  eraseCookie("sessionprefix");
  eraseCookie("nickname");
}

function printBounds(b) {
  var sw = b.getSouthWest(),
      se = b.getSouthEast(),
      ne = b.getNorthEast(),
      nw = b.getNorthWest();
  console.log([sw,se,ne,nw].toString());
}

var svg = d3.select(map.getPanes().overlayPane).append('svg');
var g = svg.append("g").attr("class", "leaflet-zoom-hide");

d3.csv("/sessions.csv", function(error, data) {
  
  data.forEach(function(d) { d.latLng = project(d); });

  var latmin = d3.min(data, function(d) { return d.latLng.x }),
      latmax = d3.max(data, function(d) { return d.latLng.x }),
      lngmin = d3.min(data, function(d) { return d.latLng.y }),
      lngmax = d3.max(data, function(d) { return d.latLng.y }), 
      bottomLeft = map.latLngToLayerPoint(new L.LatLng(latmin, lngmin)),
      topRight = map.latLngToLayerPoint(new L.LatLng(latmax, lngmax));
      console.log(bottomLeft);
      console.log(topRight);

  svg.attr("width", topRight.x - bottomLeft.x)
     .attr("height", bottomLeft.y - topRight.y)
     .style("margin-left", bottomLeft.x + "px")
     .style("margin-top", topRight.y + "px");

  g.attr("transform", "translate(" + -bottomLeft.x + "," + -topRight.y + ")");

  g.selectAll("circle").
    data(data).
    enter().
    append("circle").
    attr({
      "fill": "red",
      "r": 10,
      "cx": function(d) { return project(d).x },
      "cy": function(d) { return project(d).y }
    });;

})


// map utility
function project(x) {
    var lat = parseFloat(x.session_lat),
        lng = parseFloat(x.session_lng),
        point = map.latLngToLayerPoint(new L.LatLng(lat, lng));
    return point;
}

