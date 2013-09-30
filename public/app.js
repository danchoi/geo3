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
      rebind();
    }); 
  }

  $scope.chat = function() {

  }

  // TODO make interaction
  $scope.logout = function() {
    clearCookies();
    $scope.nickname = null;
    sessionPrefix = null;
    rebind();
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
    $http.post('/events', msg).success(rebind);
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

/* Initialize the SVG layer */
map._initPathRoot()    
var svg = d3.select('#map').select('svg');
var data, sessions;

map.on("viewreset", reset);

d3.csv("/sessions.csv", function(error, serverData) {
  data = serverData;
  sessions = svg.selectAll("circle")
    .data(data)
    .enter()
    .append("g")
    .call(reposition);

  sessions
    .append("rect")
    .attr("fill", "red")
    .attr("x", -25)
    .attr("y", -10)
    .attr("stroke", "black")
    .attr("width", 50)
    .attr("height", 10);
  sessions
    .append("text")
    .attr("x", -20)
    .attr("y", -2)
    .attr("fill", "white")
    .attr("text-align", "middle")
    .text(function (d) { return d.session_nickname });

  reset();
})


function rebind() {
  d3.csv("/sessions.csv", function(error, serverData) {
    data = serverData;
    // bind to new values
    sessions.data(data, function(d) {return d.session});
    // need to enter()
    reset();
  })
}

setInterval(rebind, 2000);
function reposition(sel) {
  sel.attr("transform", function(d) {
    var x = project(d).x;
    var y = project(d).y;
    return ("translate("+x+","+y+")")
  });
}

// Reposition the SVG to cover the features.
function reset() {
  if (!data) return;
  console.log("viewreset");
  sessions
    .transition()
      .duration(100)
      .call(reposition);
}


// map utility
function project(x) {
    var lat = parseFloat(x.session_lat),
        lng = parseFloat(x.session_lng),
        point = map.latLngToLayerPoint(new L.LatLng(lat, lng));
    return point;
}

