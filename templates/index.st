<!DOCTYPE html>
<html>
  <head>
    <title>Luach by Yesod</title>
    <script src="http://cdn.jquerytools.org/1.1.1/full/jquery.tools.min.js"></script>
    <script src="http://ajax.googleapis.com/ajax/libs/jqueryui/1.7.2/jquery-ui.min.js"></script>
    <script src="static/script.js"></script>
    <link rel="stylesheet" href="http://ajax.googleapis.com/ajax/libs/jqueryui/1.7.0/themes/start/jquery-ui.css">
    <link rel="stylesheet" href="static/style.css">
    <link rel="icon" href="static/favicon.ico">
  </head>
  <body>
    <noscript>
      <p style="color:#f00">I'm sorry, this page requires Javascript to function.</p>
    </noscript>
    <div id="wrapper">
      <div id="banner">
        <img class="logo" alt="Luach" src="static/logo.png" width="100" height="100">
        <img class="beta" alt="Beta" src="static/beta.png" width="100" height="100">
        <h1>Luach</h1>
        <h2>By Yesod Web Development</h2>
      </div>
      <div style="clear:both"></div>
      <div id="login">
        <p>Welcome to Luach, your easy-to-use <b>reminder</b>. Tell us about your <b>birthdays</b> and <b>anniversaries</b>, and we'll give you a 7-day reminder of when they'll happen next.</p>
        <p>We can give you reminders on both the <b>English</b> and <b>Hebrew</b> calendars. It's easy to get started: just log in! You don't need a special account with us, use one of the options below.</p>
        <div id="rpxnow">
            <iframe src="https://luach.rpxnow.com/openid/embed?token_url=$approot$auth/login/rpxnow/" scrolling="no" frameBorder="no">
            </iframe>
        </div>
      </div>
      <div id="interface">
        <p id="logout">Logged in as: <span id="ident"></span>. <a href="auth/logout/">Logout</a>.</p>
        <div id="information">
          <h2>Luach Anniversary Reminder</h2>
          <p id="feedlink"></p>
          <p id="subscribelink"><a rel="#subscribe" href="#">Get e-mail reminders</a></p>
          <h4>
          <p><a href="http://github.com/snoyberg/luach">See the code.</a></p>
          <p><a href="http://www.yesodweb.com/code.html">Built on Yesod Web Framework.</a></p>
          </h4>
        </div>
        <div id="upcoming">
          <h2>Upcoming Events</h2>
          <div><p>Loading...</p></div>
        </div>
        <div id="events">
          <h2>Your Events</h2>
          <p><a class="add-button" href="event/?_method_override=put">Add New Event</a></p>
          <div></div>
        </div>
        <a id="new-event-link" rel="#new-event"></a>
        <form id="new-event" method="post" action="event/?_method_override=PUT">
          <div class="tooltip"></div>
          <h2>Add new event</h2>
          <table>
            <tr>
              <td><label for="title">Title</label></td>
              <td><input id="title" type="text" name="title"></td>
            </tr>
            <tr>
              <td><label for="day">Day</label></td>
              <td><input id="day" type="text" name="day"></td>
            </tr>
            <tr>
              <td><label for="remindGreg">Remind me on English date:</label></td>
              <td>
                <input id="remindGreg" type="checkbox" name="remindGreg" checked>
                <img class="more-info" src="static/question.png" alt="More information" title="Would you like to have a reminder for this event generated on the English anniversary. In other words, if your event happend May 5, you will be reminded every May 5.">
              </td>
            </tr>
            <tr>
              <td><label for="remindHebrew">Remind me on Hebrew date:</label></td>
              <td>
                <input id="remindHebrew" type="checkbox" name="remindHebrew" checked>
                <img class="more-info" src="static/question.png" alt="More information" title="Would you like to have a reminder for this event generated on the Hebrew anniversary. In other words, if your event happend 5 Tishrei, you will be reminded every 5 Tishrei.">
              </td>
            </tr>
            <tr>
              <td><label for="afterSunset">It happened after sunset:</label></td>
              <td>
                <input id="afterSunset" type="checkbox" name="afterSunset">
                <img class="more-info" src="static/question.png" alt="More information" title="Hebrew dates start at sunset. If you're not sure, no big deal; you might be off by a day.">
              </td>
            </tr>
          </table>
          <input class='new-event-button' type="submit">
          <button id="cancel-new-event">Cancel</button>
        </form>
      </div>
      <div id="footer">This is a free service provided by <a href="http://www.yesodweb.com/">Yesod Web Development</a>. Please <a href="mailto:michael@snoyman.com">send us any questions or comments</a>.</div>
    </div>
    <div id="subscribe">
      <h1>Get reminders by e-mail</h1>
      <form action="http://www.feedmyinbox.com/feeds/verify/" method="post" id="fmi" target="fmi" onsubmit="window.open('http://www.feedmyinbox.com/', 'fmi', 'scrollbars=yes,width=500,height=490');return true;">
        <label for="email">Email Address:</label>
        <input type="text" name="email" id="email">
        <input id="feedurl" type="hidden" name="feed" value="">
        <input id="subscribe-submit" type="submit" value="Submit">
      </form>
    </div>
  </body>
</html>
