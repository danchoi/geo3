


var map = L.map('map', { dragging: false,
                         zoomControl: false,
                         zoomAnimation: false,
                         scrollWheelZoom: false,
                         doubleClickZoom: false,
                         touchZoom: false
                       }).setView([42.375, -71.106], 13);


L.tileLayer('http://{s}.tile.cloudmade.com/' + API_KEY + '/997/256/{z}/{x}/{y}.png', {
    attribution: 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery © <a href="http://cloudmade.com">CloudMade</a>',
    maxZoom: 18
}).addTo(map);


map.doubleClickZoom.disable();

var svg = d3.select(map.getPanes().overlayPane).append("svg").attr('class', 'myMapOverlay'),
    g = svg.append("g").attr("class", "leaflet-zoom");

function project(x) {
  var point = map.latLngToLayerPoint(new L.LatLng(x[0], x[1]));
  return [point.x, point.y];
}

function channelsWithLocations(x){
  return (!!x.latLng);
}

function ChatUICtrl ($scope, $http, $timeout) {

  $scope.thisUser = {};

  $scope.loginUser = function(){
   if (has_local_storage()){
      if(localStorage['geogossip.user_id'] && localStorage['geogossip.user_nick']){
        $scope.thisUser.user_nick = localStorage['geogossip.user_nick']
        $scope.thisUser.user_id = localStorage['geogossip.user_id'];
        $scope.thisUser.already_created = true;
      }
    } 
  }
  $scope.loginUser();

  $scope.createUser = function() {
    console.log("SUCCESS!");
    $http.post("/users", $scope.thisUser).success(function(data) {
      $scope.thisUser.user_nick = data.user_nick;
      $scope.thisUser.user_id = data.user_id;
      if (has_local_storage()){
        localStorage['geogossip.user_nick'] = $scope.thisUser.user_nick; 
        localStorage['geogossip.user_id'] = $scope.thisUser.user_id;
        $scope.thisUser.already_created = true;
      } 
      $scope.thisUser.submitted = true;
      console.log("this user submitted is" + $scope.thisUser.submitted);      
    });
  }

  $scope.channels = [
    // {
    //   topic: "Best icecream @ JP Licks",
    //   messages: [
    //     {
    //       name: "Dan",
    //       message: "hey everyone"
    //     },
    //     {
    //       name: "Jon",
    //       message: "I like icecream and waffles!"
    //     }
    //   ],
    //   latLng: null
    // }
  ];
  $scope.loadChannels = function (){
    $http.get("/channels").success(function(data) {
      $scope.channels = data;
      if (!$scope.activeChannel){
        $scope.selectChannel(0);
        
      }
      $scope.populateMap();
      console.log("scroll height right now is " + $(".chat_container")[0].scrollHeight);
      $(".chat_container").scrollTop($(".chat_container")[0].scrollHeight);

    });
  };

  $scope.selectChannel = function(idx){
    if(!$scope.thisUser.user_id){
      alert("Please enter a nickname");
      return;
    }
    $scope.activeChannel = $scope.channels[idx];
    $http.post('/memberships', {user_id: $scope.thisUser.user_id, channel_id: $scope.activeChannel.channel_id})
      .success(function(data){
        console.log(data);
        $(".chat_container").scrollTop($(".chat_container")[0].scrollHeight);
        $scope.activeChannel = data;
        $scope.channels[idx] = data;
        $scope.populateMap();

      });
  };

  $scope.postMessage = function(msg){
    var payload = {
      user_id: $scope.thisUser.user_id, 
      message_content: $scope.newMessage,
      channel_id: $scope.activeChannel.channel_id
    };
    $http.post("/messages", payload).success(function(data) {
      // inefficient, but OK for now
      $scope.loadChannels();
      $(".chat_container").scrollTop($(".chat_container")[0].scrollHeight);
    });
    $scope.activeChannel.messages.push({
      name: "Anon",
      message: $scope.newMessage
      
    });
    $scope.newMessage = "";
  };

  $scope.createTopic = function (topicName, latLng) {
    console.log("createTopic: "+topicName + " latLng: "+latLng);
    $http.post("/channels", {channel_title: topicName, latLng: latLng}).success(function(data) {
      console.log("created topic "+data);
      $scope.activeChannel = data;
      $scope.loadChannels();
    });

    $scope.newTopicName = "";
  };

  $scope.topicEditMode = false;
  $scope.editTopic = function(){
    $http.put("/channels/" + $scope.activeChannel.channel_id, {edited_topic_name: $scope.activeChannel.channel_title}).success(function(data) {
      console.log('edited this topic');
      console.log(data);
      $scope.topicEditMode = false;
    });

  }

  $scope.loadChannels();

  function has_local_storage(){
    try {
      return 'localStorage' in window && window['localStorage'] !== null;
    } catch (e) {
      return false;
    };
  };

  $scope.populateMap = function (){
    var xs = $.grep($scope.channels, channelsWithLocations);
    console.log("running drawMap");
    console.log(xs);
    svg.selectAll('circle').remove();
    svg.selectAll("circle")
    .data(xs)
    .enter().append("circle")
    .attr('id', function(d) { 
      return ("channel-circle-" + d.channel_id);
    })
    .attr('class', function(d) { 
      var cn = d.channel_id === $scope.activeChannel.channel_id ? "active" : null;
      return cn;
    })
    .attr("cx", function(d) {
      return (project(d.latLng)[0]);
    })
    .attr("cy", function(d) {return (project(d.latLng)[1])})
    .attr("r", 12)
    .style("fill", function(d) {
      var f = d.channel_id === $scope.activeChannel.channel_id ? "yellow" : "#f03";
      return f;
    })
    .style('fill-opacity', 0.3)
    .style("stroke", "red")
    .style("stroke-opacity", 1)
    .style("stroke-width", 3);
  };

  $scope.populateMap();

  map.on('click', function (e) {
    var latLng = [e.latlng.lat, e.latlng.lng];
    console.log(latLng);
    var topicName = prompt("Name this topic:");
    var newChannel = {
      channel_title: topicName,
      messages: [],
      latLng: latLng
    };
    $scope.$apply(function() {
      $scope.createTopic(topicName, latLng);
    });
  });

  //setup for websocket connection
  $(document).ready(function() {
    var url = "ws://" + location.host.split(':')[0] + ":9395/";
    w = new WebSocket(url);

    w.onopen = function(e) {
      console.log("Connecting to "+url);

    };
    w.onmessage = function(e){
      $scope.$apply(function(){
      
        //receceived some message
        var json_msg = JSON.parse(e.data);
        var channel_id = json_msg.channel_id;
        console.log("RECEIVED websocket message! channel_id "+channel_id);


        $scope.populateMap();
        d3.select("#channel-"+channel_id)
          .style("background-color", "red")
          .transition()
          .duration(1500)
          .style("background-color", null);
       
        
        d3.select("#channel-circle-"+channel_id)
          .style("fill", "red")
          .attr("r", 20)
          .transition()
          .duration(500)
          .attr("r", 12)
          .style("fill", function() {
            console.log(this);
            if ($(this).hasClass('channel_active')) {
              return "yellow";
            } else {
              return "transparent";
            }
          });

        
        if ($scope.activeChannel.channel_id == channel_id){
            $scope.activeChannel = json_msg.channel_obj; 
        }
      }); 
    }

  });
  

}
