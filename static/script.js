$(function(){
  $(".new-event-button").live("click", newEventButton);
  $(".delete-button").live("click", deleteButton);
  $("input[name=day]").datepicker({
    changeMonth: true,
    changeYear: true,
    dateFormat: "yy-mm-dd",
    yearRange: "-100:+5"
  });
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
  $.getJSON("feed/", showFeed);
  var h = [];
  if (! es.length) {
    h.push("<p>You have no events.</p>");
  } else {
    h.push("<table><thead><tr><th>Title</th><th>Date</th><th>Sunset</th><th>Reminders</th></tr></thead><tbody>");
    $.each(es, function(i, e){
      h.push("<tr><td>");
      h.push(e.title);
      h.push("</td><td>");
      h.push(e.day);
      h.push("</td><td>");
      h.push(e.sunset ? "After" : "Before");
      h.push("</td><td><ul>");
      $.each(e.reminders, function(i, r){
        h.push("<li>" + r + "</li>");
      });
      h.push("</ul></td><td>");
      h.push("<form method='post' action='event/" + e.uuid + "/?_method_override=delete'><input class='delete-button' type='submit' value='Delete'></form>");
      h.push("</td></tr>");
    });
    h.push("</tbody></table>");
  }
  $("#events").html(h.join(''));
}

function newEventButton() {
  $.post($(this).parent().attr("action"), {
    title: $("input[name=title]").attr("value"),
    day: $("input[name=day]").attr("value"),
    remindGreg: $("input[name=remindGreg]").attr("checked"),
    remindHebrew: $("input[name=remindHebrew]").attr("checked"),
    afterSunset: $("input[name=afterSunset]").attr("checked")
  }, showEvents, "json");
  return false;
}

function deleteButton() {
  var toDelete = confirm("Are you sure?");
  if (toDelete) {
    $.post($(this).parent().attr("action"), {}, showEvents, "json");
  }
  return false;
}

function showFeed(o) {
  var html = [];
  $.each(o, function(k, v){
    html.push("<h3>" + k + "</h3><ul>");
    $.each(v, function(i, e){
      html.push("<li>" + e.title + " &mdash; " + e.years + " years on the " + e.calendar + " calendar</li>");
    });
    html.push("</ul>");
  });
  $("#upcoming").html(html.join(''));
}
