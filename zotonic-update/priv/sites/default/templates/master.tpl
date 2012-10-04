{% extends "admin_base.tpl" %}

{% block title %} Rules {% endblock %}
{% block content %}

  {% wire id="model_form" type="submit" postback="add_master"%}
<form id="model_form" method="post" action="postback" class="row">
 <label for="message">{_ Model Definition _}</label>
   {% for p in result %}
 	
	    <textarea name="master_code" id="master_code" cols="100" rows="40" class="span12 intro" >{{p.master_code}}</textarea><br>
	    
	     {% empty %}
            
            {% endfor %}
	   {% button type="submit" class="btn btn-primary" text=_"Add Model" title=_"Test" %}
	  
</form>				
{% endblock %}
