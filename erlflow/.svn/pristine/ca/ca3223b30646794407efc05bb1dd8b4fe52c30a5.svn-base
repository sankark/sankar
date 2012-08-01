(function() {
var MainMenu = function(){
    function onClick(){
        alert('hola');
    };
    
    var aItemData = [{
        text: "<em id=\"yahoolabel\">ErlFlow!</em>",
        submenu: {
            id: "yahoo",
            itemdata: ["About ErlFlow!", "Preferences"]
        }
    
    }, {
        text: "Archivo",
        submenu: {
            id: "filemenu",
            itemdata: [{
                text: "Ver procesos...",
                helptext: "Ctrl + P",
                onclick: {
                    fn: onClick
                }
            }, {
                text: "Subir proceso...",
                helptext: "Ctrl + E",
            }, {
                text: "Editar participantes...",
                helptext: "Ctrl + U"
            }]
        }
    
    }, {
        text: "Ver",
        submenu: {
            id: "viewmenu",
            itemdata: [[{
                text: "Procesos",
                helptext: "Ctrl + V"
            }, {
                text: "Registro de sucesos",
                helptext: "Ctrl + Y",
            }], [{
                text: "Rendimiento",
                helptext: "Ctrl + X",
            }]]
        }
    
    }, {
        text: "Herramientas",
        submenu: {
            id: "toolsmenu",
            itemdata: [[{
                text: "Descargar",
                helptext: "Ctrl + Z",
                submenu: {
                    id: "descargar",
                    itemdata: ["Editor de procesos", "Editor de reportes"]
                }
            }]]
        }
    
    }, "Ay&uacute;da"];
    
    var oMenuBar = new YAHOO.widget.MenuBar("mymenubar", {
        lazyload: true,
        itemdata: aItemData
    });
    
    oMenuBar.render(document.body);
};
})();
