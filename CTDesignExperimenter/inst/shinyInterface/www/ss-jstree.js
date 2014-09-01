//input binding for jstree

//alert("In my ss-jstree.js");
var ss_jstree = new Shiny.InputBinding();

$.extend(ss_jstree, {
  find: function(scope) {
    return $(scope).find(".ss-jstree");
  },
  getValue: function(el) {
	var tree = $(el).jstree();
	var leaves = tree.get_selected(); // an array of node id's, like "j1_5".
	var i, j, r = [];
	var mynode;
	var pathlength;
	var mynode_data = [];
	var mynode_li_attr;

	// R-parse-able output string;
    for (i = 0, j = leaves.length; i < j; i++) {
      mynode = tree.get_node(leaves[i]);
      mynode_li_attr = mynode.li_attr;
    	mynode_li_attr["id"] = mynode.id;
      mynode_li_attr["text"] = mynode.text;
    	r.push([mynode_li_attr]);
    }
    return r;
  },

  setValue: function(el, value) {},
  subscribe: function(el, callback) {
    $(el).on("changed.jstree", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".ss_jstree");
  }
});
Shiny.inputBindings.register(ss_jstree);
