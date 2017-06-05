///
//  Helper functions
//

var print_session = function (session) {
    return "<p><b>" + session.id + "</b><br>" + " source type: " + session.source_type + "; status: " + session.status + "<br>";
}

var print_session_list = function (session_list) {
   if (session_list.length == 0) {
       document.getElementById("session_list").innerHTML = "No active sessions";
   }
   else {
      for (var i = 0; i < session_list.length; i++) {
          document.getElementById("session_list").innerHTML += print_session(session_list[i]);
      }
   }
}

var httpGetAsync = function(theUrl)
{
    var xmlHttp = new XMLHttpRequest();
    xmlHttp.onreadystatechange = function() {
        if (this.readyState == this.DONE && this.status == 200) {
            xmlHttp.onreadystatechange = null;
            var session_list_json = JSON.parse(xmlHttp.responseText);
            print_session_list(session_list_json);
        }
    }
    xmlHttp.open("GET", theUrl, true); // true for asynchronous
    xmlHttp.send();
}

///
//  Get Active sessions
//

httpGetAsync("session_list");
