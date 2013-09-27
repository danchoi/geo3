
function ChatController($scope, $http, $log) {

  $scope.session = {};
  $scope.submitNick = function() {
    if (!$scope.session.uuid) {
      $scope.connect($scope, $scope.nickname);
    } else {

    }
  }

  $scope.connect = function() {
    var data = "- connect "+$scope.nickname;
    console.log("connecting..");
    $http.post("/events", data).success(function(data) {
      // {"uuid":"8bc67511-a95c-48c3-a9d1-e9f8dcaaf77e","session":4} 
      $log.log(data); 
      $scope.session = data;
    }); 
  }

}

function sendEvent(s) {
  $.post("/events", s, function(data) {
    console.log(data); 
  }); 
}
$(document).ready(function() {

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
  // TODO
});


var svg = d3.select(map.getPanes().overlayPane).
            append("svg").
            attr('class', 'myMapOverlay'),
    g = svg.append("g").attr("class", "leaflet-zoom");


