%% HRL file generated by ERLSOM
%%
%% It is possible to change the name of the record fields.
%%
%% It is possible to add default values, but be aware that these will
%% only be used when *writing* an xml document.

-record('family', {anyAttribs, 'members', 'parents'}).
-record('family/parents', {anyAttribs, 'parent'}).
-record('family/parents/parent', {anyAttribs, 'name', 'child'}).
-record('family/members', {anyAttribs, 'member'}).
-record('family/members/member', {anyAttribs, 'name', 'gender'}).
