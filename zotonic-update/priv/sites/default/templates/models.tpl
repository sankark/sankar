{% extends "admin_base.tpl" %}

{% block title %} Model {% endblock %}
{% block content %}
<div id="model_div">  
  {% include "_model_form.tpl" %}
</div>
 <table class="table table-striped do_adminLinkedTable">
        <thead>
            <tr>
                <th width="20%">{_ Model Name _}</th>
                <th width="20%">{_ Model Definition _}</th>
                <th width="20%">&nbsp;</th>
            </tr>
        </thead>

        <tbody>
        
            {% for p in result %}
	    
            <tr id="{{ #li.name }}" data-href="{% url admin_edit_rsc id=p.id %}">
                <td>{{ p.model_name|default:"&nbsp;" }}</td>
                <td>{{ p.model_def|default:"&nbsp;" }}</td>
                <td>
                    <div class="pull-right">
                          {% button class="btn btn-mini" disabled=p.is_protected text="edit" action={proto_edit id=p.model_name on_success={slide_fade_out target=#li.name}} %}
                    </div>                        
                    {{ p.reversed|yesno:"reversed,&nbsp;" }}
                </td>
            </li>
            {% empty %}
            
            {% endfor %}		
{% endblock %}
