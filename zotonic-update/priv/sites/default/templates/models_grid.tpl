
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<title>jqGrid Demos</title>
  
<link rel="stylesheet" type="text/css" media="screen" href="http://trirand.com/blog/jqgrid/themes/redmond/jquery-ui-custom.css" />
<link rel="stylesheet" type="text/css" media="screen" href="http://trirand.com/blog/jqgrid/themes/ui.jqgrid.css" />
<link rel="stylesheet" type="text/css" media="screen" href="http://trirand.com/blog/jqgrid/themes/ui.multiselect.css" />
<style>
html, body {
	margin: 0;			/* Remove body margin/padding */
	padding: 0;
	overflow: hidden;	/* Remove scroll bars on browser window */	
    font-size: 75%;
}
/*Splitter style */


#LeftPane {
	/* optional, initial splitbar position */
	overflow: auto;
}
/*
 * Right-side element of the splitter.
*/

#RightPane {
	padding: 2px;
	overflow: auto;
}
.ui-tabs-nav li {position: relative;}
.ui-tabs-selected a span {padding-right: 10px;}
.ui-tabs-close {display: none;position: absolute;top: 3px;right: 0px;z-index: 800;width: 16px;height: 14px;font-size: 10px; font-style: normal;cursor: pointer;}
.ui-tabs-selected .ui-tabs-close {display: block;}
.ui-layout-west .ui-jqgrid tr.jqgrow td { border-bottom: 0px none;}
.ui-datepicker {z-index:1200;}
.rotate
    {
        /* for Safari */
        -webkit-transform: rotate(-90deg);

        /* for Firefox */
        -moz-transform: rotate(-90deg);

        /* for Internet Explorer */
        filter: progid:DXImageTransform.Microsoft.BasicImage(rotation=3);
    }

</style>


<script src="http://trirand.com/blog/jqgrid/js/jquery.js" type="text/javascript"></script>
<script src="http://trirand.com/blog/jqgrid/js/jquery-ui-custom.min.js" type="text/javascript"></script>
<script src="http://trirand.com/blog/jqgrid/js/jquery.layout.js" type="text/javascript"></script>
<script src="http://trirand.com/blog/jqgrid/js/i18n/grid.locale-en.js" type="text/javascript"></script>

<script src="http://trirand.com/blog/jqgrid/js/ui.multiselect.js" type="text/javascript"></script>
<script src="http://trirand.com/blog/jqgrid/js/jquery.jqGrid.js" type="text/javascript"></script>
<script src="http://trirand.com/blog/jqgrid/js/jquery.tablednd.js" type="text/javascript"></script>
<script src="http://trirand.com/blog/jqgrid/js/jquery.contextmenu.js" type="text/javascript"></script>
<script type="text/javascript">
	var cols=$.getJSON('http://127.0.0.1:8000/api/default/grid', function(data) {
return data.cols;
});
jQuery(document).ready(function(){

jQuery("#editgrid").jqGrid({
    
	datatype: "local",
	height: 250,
   	colNames:['Inv No','Date'],
   	colModel:[
   		{'name':cols[0],index:'user_id', width:60,editable:true,editoptions:{size:20}},
   		{'name':cols[1],index:'user_name', width:90,editable:true,editoptions:{size:30}}	
   	],
   rowNum:10,
   	rowList:[10,20,30],
   	pager: '#pagered',
   	sortname: 'user_id',
    viewrecords: true,
    sortorder: "desc",
    caption:"Editing Example",
    editurl:"someurl.php"
});
$.getJSON('http://127.0.0.1:8000/api/default/grid', function(data) {
var mydata=data.rows;
for(var i=0;i<data.total;i++)
	jQuery("#editgrid").jqGrid('addRowData',i+1,mydata[i]);
});

$("#bedata").click(function(){
	jQuery("#editgrid").jqGrid('editGridRow',"new",{height:280,reloadAfterSubmit:false});
});
});


$.ajax(
    {
       type: "POST",
       url: "SomeUrl/GetColumnsAndData",
       data: "",
       dataType: "json",
       success: function(result)
       {
            colD = result.colData;
            colN = result.colNames;
            colM = result.colModel;

            jQuery("#list").jqGrid({
                jsonReader : {
                    cell: "",
                    id: "0"
                },
                url: 'SomeUrl/Getdata',
                datatype: 'jsonstring',
                mtype: 'POST',
                datastr : colD,
                colNames:colN,
                colModel :colM,
                pager: jQuery('#pager'),
                rowNum: 5,
                rowList: [5, 10, 20, 50],
                viewrecords: true
            })
       },
       error: function(x, e)
       {
            alert(x.readyState + " "+ x.status +" "+ e.msg);   
       }
    });
setTimeout(function() {$("#list").jqGrid('setGridParam',{datatype:'json'}); },50);
</script>
</head>
<body>
<table id="editgrid" ></table>
<div id="pagered" ></div>
<input type="BUTTON" id="bedata" value="Edit Selected" />
</body>

</html>