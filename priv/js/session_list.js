///
//  Helper functions
//

var print_session = function (session) {
  return ("<tr><td>" + session.id + "</td><td>" + session.source_type + "</td><td>" + session.status + "</td></tr>");
}

var print_session_list = function (session_list) {
   if (session_list.length == 0) {
       document.getElementById("session_list").innerHTML = "No active sessions";
   }
   else {
      var session_list_node = "";
      session_list_node += "<table class=\"table\"><thead><tr><th>stream</th><th>source type</th><th>status</th></tr></thead><tbody>";
      for (var i = 0; i < session_list.length; i++) {
          session_list_node += print_session(session_list[i]);
      }
      session_list_node += "</tbody></table>";
      document.getElementById("session_list").innerHTML += session_list_node;
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
