(function() {
    var Dom = YAHOO.util.Dom,
        Event = YAHOO.util.Event,
        Sel = YAHOO.util.Selector;
        YAHOO.log('tabview.js loaded', 'info', 'tabview.js');
        //Set the time on the home screen
        erlflow.app.setTime = function() {
            var d = new Date();
            var weekday = ['Sun','Mon','Tue','Wed','Thu','Fri','Sat'];
            var h = d.getHours(), a = 'am';
            if (h >= 12) {
                a = 'pm';
                if (h > 12) {
                    h = (h - 12);
                }
            }

            var dy = d.getDate();
            if (dy < 10) {
                dy = '0' + dy;
            }

            var m = (d.getMonth() + 1);
            if (m < 10) {
                m = '0' + m;
            }

            var dt = weekday[d.getDay()] + ' ' + m + '/' + dy + '/' + d.getFullYear() + ' ' + h + ':' + d.getMinutes() + ' ' + a;
            YAHOO.util.Dom.get('datetime').innerHTML = dt;
            YAHOO.log('Setting the time/date string to: ' + dt, 'info', 'tabview.js');
        };
        
        //Method to Resize the tabview
        erlflow.app.resizeTabView = function() {
            var ul = erlflow.app.tabView._tabParent.offsetHeight;
            Dom.setStyle(erlflow.app.tabView._contentParent, 'height', ((erlflow.app.layout.getSizes().center.h - ul) - 2) + 'px');
        };
        
        //Listen for the layout resize and call the method
        erlflow.app.layout.on('resize', erlflow.app.resizeTabView);
        //Create the tabView
        YAHOO.log('Creating the main TabView instance', 'info', 'tabview.js');
        erlflow.app.tabView = new YAHOO.widget.TabView();
        //Create the Home tab       
        erlflow.app.tabView.addTab( new YAHOO.widget.Tab({
            //Inject a span for the icon
            label: '<span></span>Principal',
            id: 'homeView',
            content: '<div id="welcomeWrapper"><h2>Bienvenido a Erlflow</h2><span id="datetime"></span></div>',
            active: true
        }));
        //Create the Inbox tab
        erlflow.app.tabView.addTab( new YAHOO.widget.Tab({
            //Inject a span for the icon
            label: '<span></span>Tareas',
            id: 'inboxView',
            content: ''

        }));
        erlflow.app.tabView.on('activeTabChange', function(ev) {
            //Tabs have changed
            if (ev.newValue.get('id') == 'inboxView') {
                //inbox tab was selected
                if (!erlflow.app.inboxLoaded && !erlflow.app.inboxLoading) {
                    YAHOO.log('Fetching the inbox.js file..', 'info', 'tabview.js');
                    YAHOO.log('Inbox is not loaded yet, use Get to fetch it', 'info', 'tabview.js');
                    YAHOO.log('Adding loading class to tabview', 'info', 'tabview.js');
                    erlflow.app.getFeed();
                }
            }
            //Is an editor present?
            if (erlflow.app.editor) {
                if (ev.newValue.get('id') == 'composeView') {
                    YAHOO.log('Showing the editor', 'info', 'tabview.js');
                    erlflow.app.editor.show();
                    erlflow.app.editor.set('disabled', false);
                } else {
                    YAHOO.log('Hiding the editor', 'info', 'tabview.js');
                    erlflow.app.editor.hide();
                    erlflow.app.editor.set('disabled', true);
                }
            }
            //Resize to fit the new content
            erlflow.app.layout.resize();
        });
        //Add the tabview to the center unit of the main layout
        var el = erlflow.app.layout.getUnitByPosition('center').get('wrap');
        erlflow.app.tabView.appendTo(el);

        //resize the TabView
        erlflow.app.resizeTabView();
        //Set the time on the home screen
        erlflow.app.setTime();
        //Setup the interval to update the time
        setInterval(erlflow.app.setTime, 60000);

        
        YAHOO.log('Fetch the news feed', 'info', 'tabview.js');
        YAHOO.util.Get.script('assets/js/news.js'); 


        //When inboxView is available, update the height..
        Event.onAvailable('inboxView', function() {
            var t = erlflow.app.tabView.get('tabs');
            for (var i = 0; i < t.length; i++) {
                if (t[i].get('id') == 'inboxView') {
                    var el = t[i].get('contentEl');
                    el.id = 'inboxHolder';
                    YAHOO.log('Setting the height of the TabViews content parent', 'info', 'tabview.js');
                    Dom.setStyle(el, 'height', Dom.getStyle(erlflow.app.tabView._contentParent, 'height'));
                    
                }
            }

        });

})();
