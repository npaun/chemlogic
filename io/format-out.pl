format(Test,Symbol) --> {var(Test), !}, output(Symbol,plain).
format(_,Symbol) --> output(Symbol).

output(Symbol) --> output(Symbol,html).

output(subscript_before,plain) --> "".
output(subscript_after,plain) --> "".


output(subscript_before,html) --> "<sub>". 
output(subscript_after,html) --> "</sub>".

output(subscript_before,latex) --> "_{". 
output(subscript_after,latex) --> "}". 

output(arrow,plain) --> " --> ".
output(arrow,html) --> " &rarr; ".
output(arrow,latex) --> " \rightarrow ".
