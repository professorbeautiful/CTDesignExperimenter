<script type="text/javascript>
    var checked_ids = [];
    $('#your-tree-id').jstree("get_checked",null,true).each(function(){
        checked_ids.push(this.id);
    });
    alert(checked_ids.join(","));
</script>
