application app;
variable saisie : attribut mon_attribut;
Z : calculee restituee primrest = 0 : "";

cible test:
arguments: VAR;
application : app;
si dans_domaine(VAR, calculee *)
alors
  Z = 1;
finsi
