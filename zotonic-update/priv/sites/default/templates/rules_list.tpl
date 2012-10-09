 <table class="table table-striped do_adminLinkedTable">
        <thead>
            <tr>
                <th width="20%">{_ RuleName _}</th>
                <th width="20%">{_ Pattern _}</th>
                <th width="20%">{_ Condition _}</th>
                <th width="20%">{_ Action _}</th>
                <th width="20%">{_ ClientState _}</th>
                 <th width="10%">{_ Priority _}</th>
                <th width="20%">&nbsp;</th>
            </tr>
        </thead>
 <tbody>
 {% for p in result %}
	    
            <tr id="{{ #li.name }}" data-href="{% url admin_edit_rsc id=p.id %}">
                <td>{{ p.rule_name|default:"&nbsp;" }}</td>
                <td>{{ p.pattern|default:"&nbsp;" }}</td>
                <td>{{ p.condition|default:"&nbsp;" }}</td>
                <td>{{ p.action|default:"&nbsp;" }}</td>
                <td>{{ p.client_state|default:"&nbsp;" }}</td>
                 <td>{{ p.salience|default:"&nbsp;" }}</td>
                <td>
                    <div class="pull-right">
                        {% button class="btn btn-mini" disabled=p.is_protected text="delete" action={delete id=p.rule_name on_success={slide_fade_out target=#li.name}} %}
                         {% button class="btn btn-mini" disabled=p.is_protected text="edit" action={edit id=p.rule_name on_success={slide_fade_out target=#li.name}} %}
                    </div>                        
                    {{ p.reversed|yesno:"reversed,&nbsp;" }}
                </td>
   
            {% empty %}
            
            
            
            {% endfor %}
           