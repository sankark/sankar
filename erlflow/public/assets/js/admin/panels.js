(function(){
	//WelcomePanel - Change show text here
	function WelcomePanel(){
		var oPanel = new YAHOO.widget.Panel("exampleinfo", {
            constraintoviewport: true,
            fixedcenter: true,
            width: "400px",
            zIndex: 1
        });
        
        oPanel.setHeader("C&oacute;nsola de Administraci&oacute;n de ErlFlow");
        oPanel.setBody("Bienvenido a la c&oacute;nsola de administraci&oacute;n de ErlFlow.<br>Aqu&iacute; Ud. podr&aacute; configurar los par&aacute;metros de ejecuci&oacute;n del sistema. Si no est&aacute; seguro de lo que est&aacute; haciendo, por favor contacte al Administrador del Sistema.");	  
        oPanel.render(document.body);
	}
	
	function ProcessesPanel(){
		 /*
         Define an array of object literals, each containing
         the data necessary to create the items for a MenuBar.
         */
		/*
		var callback = {
                        success: function(oResponse){
                            YAHOO.log("XHR transaction was successful.", "info", "example");
                            YAHOO.log(oResponse.responseText);
                            var oResults = YAHOO.lang.JSON.parse(oResponse.responseText);                            
                            
                            for (var i = 0, j = oResults.networks.length; i < j; i++) {
								var obj = new Object();
								obj.id = oResults.networks[i].id;
								obj.label = oResults.networks[i].name;
                                var tempNode = new YAHOO.widget.TextNode(obj, node, false);
								tempNode.isLeaf = true;
                            }
                        },
                        
                        failure: function(oResponse){
                            YAHOO.log("Failed to process XHR transaction.", "info", "example");
                        },
                        timeout: 7000
                    };
		var sUrl = "erlflow/nets";
		YAHOO.util.Connect.asyncRequest('GET', sUrl, callback);*/
	}
})();