$( function() {
  $("#varEditPopup").dialog({
    //dialogClass: "whatever",
    buttons: [
      {
        text: "OK",
        click: function() {
            $( this ).dialog( "close" );
        }
      }
    ]
  });
});
