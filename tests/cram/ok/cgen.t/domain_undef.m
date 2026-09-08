application app;
variable saisie : attribut mon_attribut;
Z : calculee restituee primrest = 0 : "";
I : calculee primrest = 1 : "";
TAB : tableau[2] calculee primrest = 2 : "";

cible test:
application : app;
si dans_domaine(TAB[I], calculee *)
alors
  Z = 1;
finsi
