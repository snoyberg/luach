$(function(){
  $.getJSON("auth/check/", function(o){
    if (o.identifier) {
      $("#login").hide();
      $("#ident").text(o.identifier);
      $.getJSON("event/", showEvents);
    } else {
      $("#interface").hide();
    }
  });
});

function showEvents(es) {
  var h = [];
  h.push("<table><thead><tr><th>Title</th><th>Date</th><th>After sunset?</th><th>Reminders</th></tr></thead><tbody>");
  $.each(es, function(i, e){
    h.push("<tr><td>");
    h.push(e.title);
    h.push("</td><td>");
    h.push(e.day);
    h.push("</td><td>");
    h.push(e.sunset);
    h.push("</td><td>");
    h.push(e.reminders);
    h.push("</td></tr>");
  });
  h.push("</tbody></table>");
  $("#events").html(h.join(''));
}
