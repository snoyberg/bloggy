$(function(){
  var initialIndex;
  $.each($("#archives > h4"), function(i, v){
    if ($(v).text() == initialMonth) initialIndex = i;
  });
  $("#archives").tabs("#archives > ul", {
    tabs: "h4",
    effect: "slide",
    initialIndex: initialIndex
  });
});
