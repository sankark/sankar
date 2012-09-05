{% extends "admin_base.tpl" %}

{% block title %} Rules {% endblock %}
{% block content %}

  {% wire id="model_form" type="submit" postback="add_model"%}
<form id="model_form" method="post" action="postback" class="row">
 <label for="message">{_ Model Name _}</label>
<input type="text" id="model_name" name="model_name" value=""/>
 <label for="message">{_ Model Definition _}</label>
	    <textarea name="model_def" id="model_def" cols="60" rows="8" ></textarea><br>
	   {% button type="submit" class="btn btn-primary" text=_"Add Model" title=_"Test" %}
</form>		
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
                        {% button class="btn btn-mini" disabled=p.is_protected text="delete" action={delete id=p.id on_success={slide_fade_out target=#li.name}} %}
                        <a href="{% url admin_edit_rsc id=p.id %}" class="btn btn-mini">{_ edit _}</a>
                    </div>                        
                    {{ p.reversed|yesno:"reversed,&nbsp;" }}
                </td>
            </li>
            {% empty %}
            
            {% endfor %}		
{% endblock %}
