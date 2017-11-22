var page = require('webpage').create();
page.open('http://companyinfo.stock.naver.com/v1/company/c1010001.aspx?cmp_cd=071840&target=finsum_more', function () {
console.log(page.content); //page source
                   phantom.exit();
                   });
