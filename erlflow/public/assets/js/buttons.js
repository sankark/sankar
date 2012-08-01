(function() {
    var Dom = YAHOO.util.Dom,
        Event = YAHOO.util.Event;

    YAHOO.log('buttons.js loaded', 'info', 'button.js');
    //Create this loader instance and ask for the Button module
    var loader = new YAHOO.util.YUILoader({
        base: '/build/',
        require: ['button'],
        ignore: ['containercore'],
        onSuccess: function() {
            YAHOO.log('Create the search button', 'info', 'button.js');
            var searchButton = new YAHOO.widget.Button('search');
            searchButton.on('click', function() {
                var q = Dom.get('query').value;
                if (q !== 'Search the Web..') {
                    window.open('http:/'+'/search.yahoo.com/search?p=' + q);
                }
            });
            YAHOO.log('Create the Check button', 'info', 'button.js');
            var b1 = new YAHOO.widget.Button({
                label: 'Verificar',
                id: 'checkButton',
                container: Dom.get('check_buttons')
            });
            //inject a span for the icon
            var icon = document.createElement('span');
            icon.className = 'icon';
            b1.appendChild(icon);
            b1.on('click', function() {
                var t = erlflow.app.tabView.get('tabs');
                for (var i = 0; i < t.length; i++) {
                    if (t[i].get('id') == 'inboxView') {
                        erlflow.app.tabView.set('activeTab', t[i]);
                    }
                }
            });
            YAHOO.log('Create the New Message button', 'info', 'button.js');
            var b2 = new YAHOO.widget.Button({
                label: 'Nuevo',
                id: 'newButton',
                title: 'Nuevo Caso',
                container: Dom.get('check_buttons')
            });
			
            //inject a span for the icon
            var icon2 = document.createElement('span');
            icon2.className = 'icon';
            b2.appendChild(icon2);
            //Setup the click listener for the new message button
			YAHOO.util.Get.script('/assets/js/new_case.js');
			
         
            YAHOO.log('Add some functionality to the search box', 'info', 'button.js');
            Event.on('query', 'click', function() {
                this.value = '';
            });
            Event.on('query', 'blur', function() {
                if (this.value === '') {
                    this.value = 'Search the Web..';
                }
            });
        }
    });
    //Call insert, only choosing the JS files, so the skin doesn't over write my custom css
    loader.insert({}, 'js');
})();
