
output_arrow --> output_arrow(html).

output_arrow(plain) --> " --> ".
output_arrow(html)  --> " &rarr; ".
output_arrow(latex) --> " \rightarrow ".


output_subs(Function) --> output_subs(html,Function).

output_subs(plain,Function) --> call(Function).
output_subs(html,Function) --> "<sub>",call(Function),"</sub>".
output_subs(latex,Function) --> "_{",call(Function),"}".

