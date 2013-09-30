var sessionPrefix;
var COOKIE_DAYS = 3;


var map = L.map('map', { dragging: true,
                         zoomControl: true,
                         zoomAnimation: false,
                         scrollWheelZoom: false,
                         doubleClickZoom: false,
                         touchZoom: false
                       }).setView([42.375, -71.106], 14);


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

var svg = d3.select(map.getPanes().overlayPane).
            append("svg").
            attr('class', 'myMapOverlay'),
    g = svg.append("g").attr("class", "leaflet-zoom");


