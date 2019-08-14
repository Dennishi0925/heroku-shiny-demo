(function(i,s,o,g,r,a,m){
  i['GoogleAnalyticsObject']=r;
  i[r]=i[r] || 
  function(){
    (i[r].q=i[r].q||[]).push(arguments);
  },i[r].l=1*new Date();
  a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];
  a.async=1;
  a.src=g;
  m.parentNode.insertBefore(a,m);
})(window,document,'script',
   'https://www.google-analytics.com/analytics.js','ga');
ga('create', 'UA-143837983-2', 'ntucourse.herokuapp.com');
ga('send', 'pageview');


$(document).on('click', 'a', function() { ga('send', 'event', 'Link', 'click'); });
$(document).on('change', 'input', function(){  ga('send', 'event', 'Input', 'change' ); });
$(document).on('click', 'button', function() { ga('send', 'event', 'Filter', 'click'); })