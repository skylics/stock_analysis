var url ='https://www.busliniensuche.de/suche/?From=Aschaffenburg&To=Adelsheim&When=2017-11-23&ShowRidesharing=false&Company=Alle+Busunternehmen&Passengers=1&SearchMode=0&Radius=15000';
var page = new WebPage()
var fs = require('fs');


page.open(url, function (status) {
  just_wait();
});

function just_wait() {
  setTimeout(function() {
    fs.write('1.html', page.content, 'w');
    phantom.exit();
  }, 2500);
}
