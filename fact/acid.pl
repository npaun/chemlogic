acid_base("S","sulfur") --> "sulfur", !.
acid_base("P","phosphor") --> "phosphor", !.

acid_base(Sym,Base) --> element_base(Sym,Base), !.
