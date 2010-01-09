function setCurrentSlug() {
  $("#archives a").removeClass("current");
  $("#archives a[href=#" + currentSlug + "]").addClass("current");
}

$(function(){
  var initialIndex;
  var archiveLinks = $("#archives a");
  $.each(archiveLinks, function(i, a){
    var v = $(a);
    v.attr("rel", v.attr("href"))
     .attr("href", v.attr("href").replace(/.*\/([^\/]+)\/$/, "#$1"));
  });
  archiveLinks.onHash(function(event, hash){
    if (!hash) return;
    hash = hash.replace(/^#/, "");
    currentSlug = hash;
    $.getJSON(approot + "entry/" + hash + "/", function(o){
      $("title").html(o.title + " :: " + blogTitle);
      $("#article-title").html(o.title);
      $("#content").html(o.content);
    });
    setCurrentSlug();
  });
  setCurrentSlug();
  $.each($("#archives > h4"), function(i, v){
    if ($(v).text() == initialMonth) initialIndex = i;
  });
  $("#archives").tabs("#archives > ul", {
    tabs: "h4",
    effect: "slide",
    initialIndex: initialIndex
  });
});
