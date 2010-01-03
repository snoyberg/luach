$(function(){
  $(".new-event-button").live("click", newEventButton);
  $(".delete-button").live("click", deleteButton);
  $(".edit-button").live("click", editButton);
  $(".add-button").live("click", addButton);
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
      $.getJSON("settings/feedid/", setupFeedLinks);
    } else {
      $("#interface").hide();
    }
  });

  // extra information tooltips
  $("img.more-info").tooltip({
    tip: ".tooltip",
    position: "right"
  });
});

function showEvents(es) {
  $.getJSON("upcoming/", showFeed);
  var h = [];
  if (! es.length) {
    h.push("<p>You have no events.</p>");
  } else {
    h.push("<table border='0'>");
    $.each(es, function(i, e){
      h.push("<tr class='");
      h.push(i % 2 ? "odd" : "even");
      h.push("'>");
      h.push("<td>");
      h.push(e.title);
      h.push("</td><td width='80px' class='day'>");
      h.push(e.day);
      h.push("</td>");
      h.push("<td width='40px' class='controls'>");
      h.push("<a class='edit-button' href='event/" + e.uuid + "/?_method_override=put'><img src='static/edit.png' alt='Edit' title='Edit this event'></a>");
      h.push("<a class='delete-button' href='event/" + e.uuid + "/?_method_override=delete'><img src='static/delete.png' alt='Delete' title='Delete this event'></a>");
      h.push("</td>");
      h.push("</tr>");
    });
    h.push("</tbody></table>");
  }
  $("#events div").html(h.join(''));
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
    $.post($(this).parent().attr("href"), {}, showEvents, "json");
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
  $("#upcoming > div").html(html.join(''));
}

function addButton() {
  $("#new-event").show();
  return false;
}

function editButton() {
  alert("Not supported");
  return false;
}

function setupFeedLinks(o) {
  var u = o.feedUrl;
  $("#feedlink").html("<a href='" + u + "'><img src='static/feed.png' alt='News feed'></a>");
  $("#feedurl").attr("value", u);
  $("#subscribelink").show();
  var o = $("#subscribelink > a").overlay({
    api: true,
    expose: '#333'
  });
  $("#subscribe-submit").click(function(){o.close()});
}
