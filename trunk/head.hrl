-record(request, {action, heads=[], data=[], cookies=[], setcookie=[], sessionid,
        sessionbegin=false}).
-record(response, {status="200 OK", heads=[], setcookie=[], body=""}).

-record(user, {username, password}).
-record(group, {groupname, username}).
-record(message, {receiver, sender, content, time, tag}).

