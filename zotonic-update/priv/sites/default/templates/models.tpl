   <div class="tab-pane" id="{{ #tab }}-search">
	    <div class="control-group">

                <label for="input" class="control-label">{_ Use the autocompleter to search pattern. _}</label>
                <div class="controls">
                    <input id="input" class="autocompleter span5 do_autofocus" type="text" value="" />
                    <ul id="{{#suggestions}}" class="suggestions-list"></ul>
                </div>
            </div>

            {% wire id="input"
                type="click" 
                action={search
                target=#suggestions 
                action_with_id={with_args action={link subject_id=subject_id predicate="depiction" element_id=element_id} arg={object_id select_id}
                }
	        action={postback postback={reload_media rsc_id=id div_id=media_div_id} delegate="resource_admin_edit"}
                action_with_id={with_args action={zmedia_has_chosen} arg={id select_id}}
                action={dialog_close}

                cat=m.predicate.object_category["depiction"]
	        }
            %}
                
	</div>
		

