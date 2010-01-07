var eventEditApi;
var events;
$(function(){
  $(".new-event-button").live("click", newEventButton);
  $(".delete-button").live("click", deleteButton);
  $(".edit-button").live("click", editButton);
  $(".add-button").live("click", addButton);
  $("#cancel-new-event").click(cancelNewEvent);
  $("input[name=day]").datepicker({
    changeMonth: true,
    changeYear: true,
    dateFormat: "yy-mm-dd",
    yearRange: "-100:+5"
  });
  eventEditApi = $("#new-event-link").overlay({
    api: true,
    zIndex: 9998,
    closeOnClick: false,
    expose: {
        color: '#333',
        zIndex: 9997,
        closeOnClick: false
    }
  });
  $.getJSON("auth/check/", function(o){
    if (o.identifier) {
      $("#ident").text(o.displayName);
      $.getJSON("event/", showEvents);
      $.getJSON("settings/feedid/", setupFeedLinks);
      $("#interface").show();
    } else {
      $("#login").show();
    }
  });

  // extra information tooltips
  /* FIXME broken due to overlay
  $("img.more-info").tooltip({
    tip: ".tooltip",
    position: "right"
  });
  */
});

function showEvents(o) {
  var es = o.events;
  showFeed(o.upcoming);
  var h = [];
  if (! es.length) {
    h.push("<p>You have no events.</p>");
  } else {
    h.push("<table border='0'>");
    events = [];
    $.each(es, function(i, e){
      events[i] = e;
      h.push("<tr class='");
      h.push(i % 2 ? "odd" : "even");
      h.push("'>");
      h.push("<td>");
      h.push(e.title);
      h.push("</td><td width='80px' class='day'>");
      h.push(e.prettyday);
      h.push("</td>");
      h.push("<td width='40px' class='controls'>");
      h.push("<a class='edit-button' rel='" + i + "' href='event/" + e.uuid + "/?_method_override=put'><img src='static/edit.png' alt='Edit' title='Edit this event'></a>");
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
  eventEditApi.close();
  return false;
}

function deleteButton() {
  var toDelete = confirm("Are you sure?");
  if (toDelete) {
    $.post($(this).attr("href"), {}, showEvents, "json");
  }
  return false;
}

function showFeed(o) {
  var html = [];
  $.each(o, function(i, pair){
    var k = pair.day;
    var v = pair.o;
    html.push("<h3>" + k + "</h3><ul>");
    $.each(v, function(i, e){
      html.push("<li>" + e.title + " &mdash; " + e.years + " years on the " + e.calendar + " calendar</li>");
    });
    html.push("</ul>");
  });
  $("#upcoming > div").html(html.join(''));
}

function addButton() {
  $("#new-event > h2").text("Add New Event");
  $("#title").attr("value", "");
  $("#day").attr("value", "");
  $("#remindGreg").attr("checked", "checked");
  $("#remindHebrew").attr("checked", "checked");
  $("#afterSunset").removeAttr("checked");
  $("input.new-event-button").attr("value", "Add new event");
  $("#new-event").attr("action", "event/?_method_override=put");
  eventEditApi.load();
  return false;
}

function editButton() {
  var e = events[$(this).attr("rel")];
  $("#new-event > h2").text("Edit Event");
  $("#title").attr("value", e.rawtitle);
  $("#day").attr("value", e.day);
  var remindGreg = false, remindHebrew = false;
  $.each(e.reminders, function(i, v) {
    if (v == "Gregorian") {
      remindGreg = true;
    } else if (v == "Hebrew") {
      remindHebrew = true;
    }
  });
  if (remindGreg) {
      $("#remindGreg").attr("checked", "checked");
  } else {
      $("#remindGreg").removeAttr("checked");
  }
  if (remindHebrew) {
      $("#remindHebrew").attr("checked", "checked");
  } else {
      $("#remindHebrew").removeAttr("checked");
  }
  if (e.sunset == "true") {
      $("#afterSunset").attr("checked", "checked");
  } else {
      $("#afterSunset").removeAttr("checked");
  }
  $("input.new-event-button").attr("value", "Change event");
  $("#new-event").attr("action", $(this).attr("href"));
  eventEditApi.load();
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
  $("#subscribe > form").submit(function(){o.close()});
}

function cancelNewEvent() {
  eventEditApi.close();
  return false;
}
