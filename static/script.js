$(function(){
  var initialIndex;
  var archiveLinks = $("#archives a");
  $.each(archiveLinks, function(i, a){
    var v = $(a);
    var s = v.attr("href").replace(/.*\/([^\/]+)\/$/, "$1");
    if (s == currentSlug) v.addClass("current");
  });
  $.each($("#archives > h4"), function(i, a){
    var v = $(a);
    v.prepend("<div></div>");
    if (v.text() == initialMonth) initialIndex = i;
  });
  $("#archives").tabs("#archives > ul", {
    tabs: "h4",
    effect: "slide",
    initialIndex: initialIndex
  });
});
