$(function () {
  $("#scenarioTree").jstree({
    "types" : {
      "level_1" : {
            'icon' : { 'image' :  "BLOCK32.png" }
      },
      "level_2" : {
            'icon' : { 'image' :  "glyphicon glyphicon-flash" }
      }
    },
    // "plugins" : [ "types", "contextmenu", "wholerow" ]
    "plugins" : [ "wholerow", "contextmenu", "types", "html_data"]
  });
});
