$(function () {
  $("#scenarioTree").jstree({
    "types" : {
    "types" : {
      "level_1" : {
            'icon' : { 'image' :  url('BLOCK32.png') }
      },
      "level_2" : {
            'icon' : { 'image' :  url('Insert.png') }
      }
    },
    },
    // "plugins" : [ "types", "contextmenu", "wholerow" ]
    "plugins" : [ "contextmenu", "types"],
	"contextmenu": {
	    "items": {
		"create" : false,
		"ccp" : false,
		"rename" : false,
		"remove" : {
		    "label" : "&nbsp;Delete",
		    "icon" : "/images/icon/cross.png"
		}
	    }
	},
  });
});
