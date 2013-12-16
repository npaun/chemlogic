

output(output,Type) --> output(html,Type).

output(user,arrow) --> " --> ".
output(html,arrow) --> " &rarr; ".
output(latex,arrow) --> " \\rightarrow ".

output(user,sub_start) --> "".
output(user,sub_end) --> "".

output(html,sub_start) --> "<sub>".
output(html,sub_end) --> "</sub>".

output(latex,sub_start) --> "_{".
output(latex,sub_end) --> "}".
