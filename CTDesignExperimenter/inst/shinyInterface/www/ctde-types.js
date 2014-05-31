$(function () {
  $("#jstree1").jstree({
    "types" : {
      "level_1" : {
        "icon" : "www/BLOCK.gif"
      }
    },
    "plugins" : [ "types", "contextmenu", "wholerow" ]
  });
});
